#++
(ql:quickload '3b-openxr-generator)
(in-package #:3b-openxr-parse-spec)

;; generate low-level bindings

(defvar *package-name* "3b-openxr")
(defvar *mid-level-package-name* "3b-openxr-mid-level")
(defvar *bindings-package-name* "3b-openxr-bindings")
(defvar *core-definer* 'defcfun)
(defvar *ext-definer* 'defextfun)
(defvar *command-definer* (make-hash-table))
(defvar *ext-fun-index* 0)

(defvar *api-version* nil)
(defvar *api-last-updated* nil)
(defvar *exports* (make-hash-table :test 'equalp))
(defvar *enum-values* (make-hash-table :test 'equalp))
(defvar *printed-structs* (make-hash-table))
(defvar *constants* (make-hash-table :test 'equal))
(defvar *known-commands* (make-hash-table))
(defvar *named-constants* nil)
(defvar *print-constants* nil)
(defvar *constants-to-print* nil)

(defparameter *skip-struct*
  (a:plist-hash-table '(;; float/int vectors
                        ;; vector-2f t
                        ;; vector-3f t
                        ;; vector-4f t
                        ;; color-4f t
                        ;; quaternion-f t
                        ;; offset-2d-f t
                        ;; extent-2d-f t
                        ;; offset-2d-i t
                        ;; extent-2d-i t
                        ;; composites, not sure about these yet
                        pose-f nil
                        rect-2d-i nil
                        rect-2d-i nil)))


(defvar *spec-dir* (asdf:system-relative-pathname '3b-openxr "spec/"))

(defvar *vendor-ids* (make-hash-table :test 'equal))
(defvar *tags* (make-hash-table :test 'equal))


(defparameter *defines*
  ;; some manually extracted #defines, not sure if these should be
  ;; +foo+ style or :foo style? need to manually adjust applicable arg
  ;; types for latter, so ++ for now
  '(+null-path+ 0
    +null-system-id+ 0
    +no-duration+ 0
    +infinite-duration+ #x7fffffffffffffff
    +min-haptic-duration+ -1
    +frequency-unspecified+ 0
    +hand-joint-count-ext+ 26))

(defparameter *define-types*
  (alexandria:plist-hash-table '() :test 'equal))


(defparameter *bitmask-types* (make-hash-table))
(defparameter *enum-types* (make-hash-table))
(defparameter *struct-types* (make-hash-table))
(defparameter *struct-dependencies* (make-hash-table))

(defun extract-array-size (str)
  (when (position #\[ str)
    (assert (= 1 (count #\[ str)))
    (subseq str (1+ (position #\[ str)) (position #\] str))))

(defun add-export (name)
  (setf (gethash name *exports*) t))

(defun add-constant (name value)
  (assert (or (not (nth-value 1 (gethash name *constants*)))
              (equalp value (gethash name *constants*))))
  (setf (gethash name *constants*) value))

(defun get-constant (name)
  (gethash name *constants*))

(defun print-comment (x &key prefix)
  (when (header-comment x)
    (map 'nil (lambda (c) (format t "~&~a" c)) (reverse (header-comment x))))
  (when (and (typep x 'has-comment)
             (slot-boundp x 'comment)
             (comment x))
    (if (stringp (comment x))
        (format t "~&~a" (format-comment (comment x) :prefix prefix))
        (map 'nil (lambda (c)
                    (format t "~&~a" (format-comment c :prefix prefix)))
             (comment x)))))

(defun get-types (registry types)
  (remove-if-not (a:rcurry 'typep types)
                 (types registry)))

(defun print-define (d)
  (unless (and (gethash (name d) *defines-xmlval*)
               (string= (text d) (gethash (name d) *defines-xmlval*)))
    (break "added or changed define ~s~% ~a~% was~%~a" (name d) (text d)
           (gethash (name d) *defines-xmlval*)))
  (case (name d)
    (:current-api-version
     (let ((o (position #\( (text d)))
           (c (position #\) (text d))))
       (setf *api-version*
             (mapcar 'parse-integer
                     (split-sequence:split-sequence
                      #\, (subseq (text d) (1+ o) c)))))
     (print-comment d)
     (add-export (a:format-symbol '3b-openxr-parse-spec "+~a+" (name d)))
     (format t "~&~((defconstant +~a+ ~a)~)" (name d) `(make-version ,@*api-version*)))
    ;; constants that are easier to define manually
    ((:null-handle)
     (print-comment d)
     (add-export (a:format-symbol '3b-openxr-parse-spec "+~a+" (name d)))
     (format t "~&~((defconstant +~a+ ~a)~)" (name d) 0))
    (:max-event-data-size
     (print-comment d)
     (add-export (a:format-symbol '3b-openxr-parse-spec "+~a+" (name d)))
     (format t "~&~((defconstant +~a+ #.(cffi:foreign-type-size '(:struct event-data-buffer)))~)" (name d)))
    ;; constants that can be used directly
    ((:null-path
      :null-system-id
      :min-haptic-duration
      :no-duration
      :infinite-duration
      :frequency-unspecified
      :min-composition-layers-supported
      :hand-joint-count-ext
      :null-controller-model-key-msft
      :null-render-model-key-fb
      :facial-expression-eye-count-htc
      :facial-expression-lip-count-htc
      :hand-forearm-joint-count-ultraleap
      :face-expression-lips-toward-fb
      :max-haptic-amplitude-envelope-samples-fb
      :max-haptic-pcm-buffer-size-fb)
     (print-comment d)
     (add-export (a:format-symbol '3b-openxr-parse-spec "+~a+" (name d)))
     (let ((v (numeric-value
               (subseq (text d) (1+(position #\space (text d) :from-end t))))))
       (if (search "0x" (text d))
           (format t "~&~((defconstant +~a+ #x~x)~)" (name d) v)
           (format t "~&~((defconstant +~a+ ~a)~)" (name d) v))))

    ;; macros for dealing with version #s, just generate functions
    (:make-version
     (print-comment d)
     (add-export 'make-version)
     (format t "~&(declaim (inline make-version))")
     (format t "~&~(~s~)"
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defun make-version (major minor patch)
                  "Translate MAJOR,MINOR,PATCH to an XR Version value"
                  (check-type major (unsigned-byte 16))
                  (check-type minor (unsigned-byte 16))
                  (check-type patch (unsigned-byte 32))
                  (logior (ash (ldb (byte 16 0) major) 48)
                          (ash (ldb (byte 16 0) minor) 32)
                          (ldb (byte 32 0) patch)))))
     ;; add an inverse function in addition to the accessors from spec
     (add-export 'parse-version)
     (format t "~&~(~s~)"
             `(defun parse-version (version)
                "Return components of an XR version as 3 values: MAJOR, MINOR, PATCH "
                (values (ldb (byte 16 48) version)
                        (ldb (byte 16 32) version)
                        (ldb (byte 32 0) version)))))
    ((:version-major :version-minor :version-patch)
     (destructuring-bind (w o) (getf '(:version-patch (32 0)
                                       :version-minor (16 32)
                                       :version-major (16 48))
                                     (name d))
       (print-comment d)
       (add-export (name d))
       (format t "~&(declaim (inline ~(~a~)))" (name d))
       (format t "~&~(~a~)"
               `(defun ,(name d) (version)
                  (ldb (byte ,w ,o) version)))))
    ;; macro for checking result
    ((:succeeded :unqualified-success :failed)
     ;; todo: define CL versions of these once type of XrResult is decided
     )
    (:face-expresssion-set-default-fb
     ;; typo fix in spec, not needed since we should have no old code
     ;; using old name
     )
    ((:may-alias :define-handle :define-atom)
     ;; ignore some things only used for making C definitions
     )
    (t
     (break "todo: unknown #define ~s?~%~a" (name d) (text d)))))

(defun print-basetype (i)
  (print-comment i)
  (let ((name (name i))
        (type (type i)))
    (add-export name)
    (format t "~&~((defctype ~s ~s)~)~%~%" name type)))

(defun print-handle (i)
  (print-comment i)
  (let ((name (name i))
        (type (type i)))
    (Assert (eql type :define-handle))
    (add-export name)
    (format t "~&~((defctype ~s xr-handle)~)~%~%" name)))

(defun collect-enums (registry)
  (clrhash *enum-types*)
  (clrhash *bitmask-types*)
  (clrhash *enum-values*)
  (clrhash *constants*)
  (loop for e across (enums registry)
        do (setf (gethash (name e) *enum-types*) (list e))
           (when (slot-boundp e 'enum)
             (loop for i across (enum e)
                   do (Assert (not (gethash (raw-name i) *enum-values*)))
                      (setf (gethash (raw-name i) *enum-values*)
                            (value i))
                      (add-constant (raw-name i) (value i)))))
  (setf (gethash :globals *enum-types*) (list nil))
  (loop for x across (extensions *spec*)
        do (unless (string= (supported x) "disabled")
             (loop for r across (require x)
                   do (when (slot-boundp r 'enum)
                        (loop for e across (enum r)
                              for extends = (if (slot-boundp e 'extends)
                                                (extends e)
                                                :globals)
                              when (and (slot-boundp e 'alias))
                                do (assert (gethash (alias e) *enum-values*))
                                   (setf (slot-value e 'value)
                                         (gethash (alias e)
                                                  *enum-values*))
                              when extends
                                do (let ((base (gethash extends *enum-types*)))
                                     (assert base)
                                     (push e (cdr base))
                                     (Assert (not (gethash (name e) *enum-values*)))
                                     (setf (gethash (name e) *enum-values*)
                                           (value e))
                                     (add-constant (raw-name e) (value e)))))))))

(defun collect-structs (registry)
  (clrhash *printed-structs*)
  (clrhash *struct-types*)
  (clrhash *struct-dependencies*)
  (setf *named-constants* nil)
  ;; find all struct type names
  (loop for s across (get-types registry 'type/struct)
        do (setf (gethash (name s) *struct-types*) s))
  ;; find constants used for array sizes, and add dependencies for any
  ;; struct types in members
  (loop for s across (get-types registry 'type/struct)
        when (slot-boundp s 'member)
          do (loop for m across (member s)
                   ;; should look for <enum>, but some are missing, so
                   ;; just search for []
                   for size = (extract-array-size (text m))
                   when (and size (get-constant size))
                     do (push size *named-constants*)
                   when (gethash (type m) *struct-types*)
                     do (push (type m)
                              (gethash (name s) *struct-dependencies*)))))

(defun print-enum (e)
  (destructuring-bind (definition &rest ext) (gethash (name e) *enum-types*)
    (assert definition)
    ;; we print the actual bitfield type from types/bitmask, so don't
    ;; print the -bits version from enums
    (when (eql (type definition) :bitmask)
      ;; probably should check to make sure it is actually used by a
      ;; types/bitmask, but for now just making sure the name looks
      ;; close enough
      (assert (search "FlagBits" (raw-name definition))))
    (unless (eql (type definition) :bitmask)
      (print-comment e)
      (print-comment definition)
      (add-export (name definition))
      (if (eql (name definition) 'result)
          (progn
            ;; we don't want cffi translation of XrResult, so define
            ;; it as int and define the actual enum as %result
            (format t "~&(defctype result :int)~%")
            (format t "~&~((defcenum ~a~)" '%result))
          (format t "~&~((defcenum ~a~)" (name definition)))
      (when (slot-boundp definition 'enum)
        (loop for v across (enum definition)
              do (print-comment v :prefix "  ")
                 (cond
                   ((eql (name definition) 'result)
                    ;; print XrResult as constants instead of
                    ;; keywords, since translating from numbers to
                    ;; keywords is ambiguous when enums are promoted,
                    ;; and it loses the qualified success / error info
                    ;; from the sign of the value.
                    (add-export (name v))
                    (format t "~&  (~(~a ~a~))" (name v) (value v)))
                   (t
                    (format t "~&  (~(~s ~a~))" (name v) (value v))))))
      (loop for e in (reverse ext)
            do (print-comment e)
               (cond
                 ((eql (name definition) 'result)
                  (add-export (name e))
                  (format t "~&  (~(~a ~a~))" (name e) (value e)))
                 (t
                  (format t "~&  (~(:~a ~a~))" (name e) (value e)))))
      (format t ")~%~%"))))

(defun print-bitfield (e)
  (destructuring-bind (definition &rest ext) (or (gethash (name e) *enum-types*)
                                                 (gethash (%translate-type-name
                                                           (bitvalues e))
                                                          *enum-types*))
    (assert definition)
    (print-comment e)
    (print-comment definition)
    (add-export (name e))
    (if (type e)
        (format t "~&~((defbitfield* (~s ~s)~)" (name e) (type e))
        (format t "~&~((defbitfield* ~a~)" (name e)))
    (when (slot-boundp definition 'enum)
      (loop for v across (enum definition)
            do (print-comment v :prefix "  ")
               (format t "~&  (~(~s #x~8,'0x~))" (name v) (value v))))
    (loop for e in (reverse ext)
          do (print-comment e :prefix "  ")
             (format t "~&  (~(:~a #x~8,'0x~))" (name e) (value e)))
    (format t ")~%~%")))

(defun maybe-slot (o slot)
  (when (slot-boundp o slot) (slot-value o slot)))

(defun print-struct-member (m)
  (print-comment m :prefix "  ")
  (let ((enum (maybe-slot m 'enum))
        (len (maybe-slot m 'len))
        (optional (maybe-slot m 'optional))
        (noautovalidity (maybe-slot m 'noautovalidity))
        (values (maybe-slot m 'values)))
    (add-export (name m))
    (when len
      (format t "~&  ;; length =~(~{ ~s~}~)" (coerce len 'list)))
    (when values
      ;; if this starts being used for more than 'type' field, make
      ;; sure it is still doing the right thing
      (assert (equal (raw-type m) "XrStructureType"))
      (assert (and (stringp values) (not (search "," values))))
      (format t "~&  ;; values = ~s" (make-enum-name values (raw-type m))))
    (let ((array (position #\[ (text m)))
          (pointer (position #\* (text m)))
          (type (type m)))
      (when enum
        (assert array))
      (when len
        (assert pointer))
      (when (gethash type *struct-types*)
        (setf type `(:struct ,type)))
      (cond
        ((and array pointer)
         (break "array + pointer"))
        (pointer
         (let ((pointers (count #\* (text m))))
           (when (and (eql type :char))
             (eql (a:last-elt len) :null-terminated)
             (setf type :string)
             (decf pointers))
           (loop for i below pointers do (setf type `(:pointer ,type)))
           (format t "~&  ~((~a ~s)~)" (name m) type)))
        ((and array enum)
         (format t "~&  ;; length = +~a+" (make-enum-name enum nil))
         (format t  "~&  ~((~a ~s :count ~a)~)" (name m) type
                 (or (get-constant enum)
                     (numeric-value enum))))
        (array
         (let ((v (extract-array-size (text m))))
           (when (get-constant v)
             (format t "~&  ;; length = +~a+" (make-enum-name v nil)))
           (if (get-constant v)
               (format *debug-io* "~&array member missing <enum> tag? ~s ~s~%"
                       (name m) v)
               (format *debug-io* "~&array member with numeric literal size? ~s ~s~%"
                       (name m) v))
           (format t  "~&  ~((~a ~s :count ~a)~)" (name m) type
                   (or (get-constant v)
                       (numeric-value v)))))
        (t
         (format t "~&  ~((~a ~s)~)" (name m) type)))
      (when (or optional noautovalidity)
        (format t " ;;")
        (when optional
          (assert (equalp optional "true"))
          (format t " optional"))
        (when (and optional noautovalidity)
          (format t ","))
        (when noautovalidity
          (assert (equalp noautovalidity "true"))
          (format t " noautovalidity"))
        (format t "~%")))))

(defun print-struct (s &key override-name)
  (format *debug-io* "print ~s @ ~s (~s)~%" (name s) override-name
          (gethash (or override-name (name s)) *printed-structs*))
  (when override-name
    (format *debug-io* " <- ~s~%" (name s)))
  (unless (gethash (or override-name (name s)) *printed-structs*)
    (setf (gethash (or override-name (name s)) *printed-structs*) t)
    (setf (gethash (or override-name (name s)) *struct-types*) t)

    (map 'nil (lambda (a)
                (unless (gethash a *printed-structs*)
                  (format *debug-io* ";; print dep ~s (~s) from ~s~%"
                          a (gethash a *printed-structs*) (name s))
                  (print-struct (gethash a *struct-types*))))
         (gethash (or override-name (name s)) *struct-dependencies*))
    (print-comment s)
    (add-export (name s))
    (if (slot-boundp s 'alias)
        (progn
          (format t "~&;; alias struct ~s~%;;           -> ~s~%"
                  (name s) (alias s))
          ;; don't seem to be able to define an alias that works with
          ;; (:struct alias), which would be inconsistent when real
          ;; name needs (:struct ...), so just redefine the whole
          ;; structure with the new name
          (let ((a (gethash (alias s) *struct-types*)))
            (setf (gethash (or override-name (name s)) *printed-structs*) nil)
            (unless a
              (break "couldn't find definition for aliased struct?~%~s -> ~s"
                     (name s) (alias s)))
            (print-struct a :override-name (name s))))
        (let ((parent (when (slot-boundp s 'parentstruct) (parentstruct s)))
              (extends (when (slot-boundp s 'structextends) (structextends s)))
              (protect (when (slot-boundp s 'protect) (protect s)))
              (alias (when (slot-boundp s 'alias) (alias s)))
              (mayalias (when (slot-boundp s 'mayalias) (mayalias s)))
              (returnedonly (when (slot-boundp s 'returnedonly) (returnedonly s))))
          (when parent (format t "~&;;  parent : ~a~%"
                               (%translate-type-name parent)))
          (when extends (format t "~&;;  extends : ~a~%"
                                (%translate-type-name extends)))
          (when protect (format t "~&;;  protect : ~a~%" protect))
          (when alias (format t "~&;;  alias : ~a~%" alias))
          (when mayalias (format t "~&;;  mayalias : ~a~%" mayalias))
          (when returnedonly (format t "~&;;  returned only : ~a~%" returnedonly))
          (format t "~&(defcstruct ~(~a~)" (or override-name (name s)))
          (setf (gethash (name s) *struct-types*) s)
          (loop for i across (member s)
                do (print-struct-member i))
          (format t ")~%~%")))))

(defun print-funcpointer (p)
  (print-comment p)
  ;; todo: might be nice to parse the c def and generate an example
  ;; defcallback form to put in comment instead of just the c
  ;; (possibly even generate a defining macro?). Only 2 so far though,
  ;; so not bothering for now.
  (format t "~&~a" (format-comment (text p)))
  (add-export (name p))
  (format t "~&~((defctype ~a :pointer)~)~%~%" (name p)))

(defun print-constant (e &key (mark ""))
  (print-comment e)
  ;; possibly should put ++ on at least some of these? -spec-version
  ;; and -spec-name, array sizes? not really expecting people to
  ;; import this package though, and most aren't short enough to be
  ;; confused with user variables anyway, so not sure.
  (if (and (stringp (value e))
           (char= #\" (char (value e) 0)))
      ;; print string 'constant's with defvar to avoid redefinition issues
      (format t "~&(defparameter ~(~a~a~a~) ~a)" mark (name e) mark (value e))
      (format t "~&~((defconstant ~a~a~a ~a)~)" mark (name e) mark (value e)))
  (if mark
      (add-export (a:format-symbol '3b-openxr-parse-spec
                                   "~a~a~a" mark (name e) mark))
      (add-export (name e))))

(defun print-types (registry)
  (format t ";; globals without an enum")
  (format t "~%")
  (loop for d across (types registry)
        do (etypecase d
             (type/basetype
              (print-basetype d))
             (type/handle
              (print-handle d))
             (type/enum
              (print-enum d))
             (type/bitmask
              (print-bitfield d))
             (type/funcpointer
              (print-funcpointer d))
             (type/struct
              ;; printed separately so it can depend on other types
              ;; without reordering them
              )
             (type/include
              )
             (type/define
              (if *print-constants*
                  (print-define d)
                  (push d *constants-to-print*)))
             (type/requires
              )))
  (loop for d across (get-types registry 'type/struct)
        unless (gethash (name d) *skip-struct*)
          do (print-struct d)))


(defun print-param (p)
  (print-comment p :prefix "  ")
  (let ((enum (maybe-slot p 'enum))
        (len (maybe-slot p 'len))
        (optional (maybe-slot p 'optional))
        (externsync (maybe-slot p 'externsync))
        (array (position #\[ (text p)))
        (pointer (position #\* (text p)))
        (type (type p)))
    (when len
      (format t "~&  ;; length =~(~{ ~s~}~)" (coerce len 'list)))
    (when optional
      (format t "~&  ;; optional = ~s" optional))
    (when externsync
      (format t "~&  ;; externsync = ~s" externsync))
    (when enum
      (assert array))
    (when len
      (assert pointer))
    (when (gethash type *struct-types*)
      (setf type `(:struct ,type)))
    (cond
      ((and array pointer)
       (break "array + pointer"))
      (pointer
       (let ((pointers (count #\* (text p))))
         (when (and (eql type :char))
           (eql (a:last-elt len) :null-terminated)
           (setf type :string)
           (decf pointers))
         (loop for i below pointers do (setf type `(:pointer ,type)))
         (format t "~&  ~((~a ~s)~)" (name p) type)))
      ((and array enum)
       (format t "~&  ;; length = +~a+ (~a)" (make-enum-name enum nil)
               (or (get-constant enum)
                   (numeric-value enum)))
       (format t  "~&  ~((~a (:pointer ~s))~)" (name p) type))
      (array
       (break "todo: param array without enum?"))
      (t
       (format t "~&  ~((~a ~s)~)" (name p) type)))))

(defun parse-implicitexternsync-param (m)
  (let ((pname nil)
        (flink nil)
        (slink nil)
        (text nil))
    (setf text
          (with-output-to-string (*standard-output*)
            (loop with prev = 0
                  for i = (position #\: m) then (position #\: m :start (1+ i))
                  while i
                  do (let* ((b (position #\space m :end i :from-end t))
                            (f (or (position-if-not
                                    (lambda (x)
                                      (or (alphanumericp x)
                                          (position x "_")))
                                    m :start (1+ i))
                                   (length m)))
                            (type (subseq m (1+ b) i))
                            (name (subseq m (1+ i) f)))
                       (format t "~a" (subseq m prev (1+ b)))
                       (setf prev f)
                       (ecase (a:make-keyword (string-upcase type))
                         (:pname
                          (assert (not pname))
                          (setf pname (translate-var-name name))
                          (format t "~a" pname))
                         (:flink
                          (push (%translate-type-name name) flink)
                          (format t "~a call" (car flink)))
                         (:slink
                          (push (%translate-type-name name) slink)
                          (format t "~a object" (car slink))))))))
    (cl:values pname flink slink text)))

(defun print-command (c &key override-name override-raw-name)
  (print-comment c)
  (assert (not (a:xor override-name override-raw-name)))
  (format t
          (format-comment (format nil "success codes:~( ~s~)~%"
                                  (coerce (success-codes c) 'list))))
  (format t
          (format-comment (format nil "  error codes:~( ~s~)~%"
                                  (coerce (error-codes c) 'list))))
  (when (or (eql (name c) 'set-input-device-location-ext)
            (eql (name c) 'set-input-device-state-vector-2f-ext))
    ;; skip 2 functions that would need libffi for now
    (format t "~&#++~%"))
  (let ((implicit-extern-sync (make-hash-table)))
    (when (slot-boundp c 'implicitexternsyncparams)
      (loop for i across (implicitexternsyncparams c)
            do (loop for j across (param i)
                     do (multiple-value-bind (p flink slink text)
                            (parse-implicitexternsync-param j)
                          (setf (gethash p implicit-extern-sync)
                                (list flink slink text))))))
    (add-export (or override-name (name c)))
    ;; the ~s needs to preserve case since it is the C name, others get ~(~)
    (unless (gethash (name c) *command-definer*)
      (break "don't know how to define ~s?~%~a"
             (name c) c))
    (format t "~&(~(~a~) (~s ~(~a~)~@[ ~a~]) ~(~a~)"
            (gethash (name c) *command-definer* "????")
            (or override-raw-name (raw-name c))
            (or override-name (name c))
            (when (equal (gethash (name c) *command-definer*)
                         *ext-definer*)
              (shiftf *ext-fun-index* (1+ *ext-fun-index*)))
            (type c))
    (loop for p across (param c)
          for ies = (gethash (name p) implicit-extern-sync)
          do (when ies
               (format t "~&  ~a" (format-comment
                                   (format nil "Implicit external sync: ~a"
                                           (third ies))))
               (remhash (name p) implicit-extern-sync))
             (print-param p))
    (assert (zerop (hash-table-count implicit-extern-sync))))
  (format t ")~%~%"))

(defun print-commands (registry)
  (clrhash *known-commands*)
  (setf *ext-fun-index* 0)
  (loop for i across (commands registry)
        do (etypecase i
             (command/alias
              (format t "~&;; alias function ~s -> ~s~%"
                      (name i) (alias i))
              (print-comment i)
              (assert (gethash (alias i) *known-commands*))
              (print-command (gethash (alias i) *known-commands*)
                             :override-name (name i)
                             :override-raw-name (raw-name i)))
             (command
              (setf (gethash (name i) *known-commands*) i)
              (print-command i)))))

(defun collect-command-definers (registry)
  (clrhash *command-definer*)
  (loop for e across (extensions registry)
        do (loop for r across (require e)
                 do (when (slot-boundp r 'command)
                      (loop for c across (command r)
                            do (setf (gethash (%translate-type-name c)
                                              *command-definer*)
                                     *ext-definer*)))))
  (when (/= 1 (length (feature registry)))
    ;; if we have 0 FEATURE something is wrong. If more than 1, figure
    ;; out if we can just export everything at once or need to split
    ;; into multiple systems (for example if a new major version
    ;; redefines a function or constant incompatibly)
    (error "~s core versions found, decide what this should do..."
           (length (feature registry))))
  (loop for e across (feature registry)
        do (loop for r across (require e)
                 do (when (slot-boundp r 'command)
                      (loop for c across (command r)
                            do (setf (gethash (%translate-type-name c)
                                              *command-definer*)
                                     *core-definer*))))))

(defparameter *struct-defaults*
  (a:plist-hash-table
   '(debug-utils-messenger-create-info-ext (user-data 0
                                            user-callback nil
                                            ;; possibly should default to all?
                                            message-severities nil
                                            message-types nil)
     action-set-create-info (priority 0)
     action-space-create-info (subaction-path "%:+null-path+")
     action-state-get-info (subaction-path "%:+null-path+")
     haptic-action-info (subaction-path "%:+null-path+")
     active-action-set (subaction-path "%:+null-path+"))))

(defparameter *struct-casts*
  (a:plist-hash-table
   '(debug-utils-messenger-create-info-ext (user-data cast-int-pointer
                                            user-callback cast-callback)
     reference-space-create-info (pose cast-pose))))


(defun print-with-struct (struct &key (name (name struct))
                                   (defaults (gethash name *struct-defaults*))
                                   (casts (gethash name *struct-casts*)))
  (when (or (not (slot-boundp struct 'member))
            (loop for m across (member struct)
                    thereis (and (eql (name m) 'type)
                                 (not (slot-boundp m 'values)))))
    ;; skip the 'parent class' structs with `type` member but no
    ;; value, and any alias that accidentally get here
    (return-from print-with-struct nil))
  (let ((w (a:format-symbol '#:3b-openxr-parse-spec
                            "WITH-~a" name))
        (slots (map 'list 'name (member struct)))
        (-p (make-hash-table)))
    (flet ((make-key (n)
             (let* ((nd '#:no)
                    (d (getf defaults n nd)))
               (when (eql n 'next) (setf d "'(cffi:null-pointer)"))
               (if (eql d nd)
                   ;; no default specified, use -p
                   (progn
                     (setf (gethash n -p)
                           (a:format-symbol '#:3b-openxr-parse-spec
                                            "~a-p" n))
                     (list n nil (gethash n -p)))
                   ;; default specified, just use that
                   (list n d))))
           (print-set (n)
             (let* ((p (gethash n -p))
                    (c (getf casts n)))
               (cond
                 ;; no default specified, use -p arg
                 ((and p c)
                  (format t "~(,@(when ~a `(%:~a (~a ,~a)))~)"
                          (gethash n -p) n c n))
                 (p
                  (format t "~(,@(when ~a `(%:~a ,~a))~)" (gethash n -p) n n))
                 ;; default specified, just use that
                 (c
                  (format t "~(%:~a (~a ,~a)~)" n c n))
                 (t
                  (format t "~(%:~a ,~a~)" n n))))))
      (format t "~&~%~(~@<(~;defmacro ~a ~:<~:<~a ~a~-2:i~@{ ~:_~a~}~:>~-1i~:_ ~a ~a~:>~:>~)~%"
              w `((pointer &key
                           ;; option to put @body within
                           ;; with-foreign-slots scope
                           %slots
                           ,@(mapcar #'make-key (remove 'type slots)))
                  &body body))
      (format t "  `(cffi:with-foreign-object (,pointer '(:struct ~(%:~a~)))~%"
              name)
      (format t "     (cffi:with-foreign-slots ~((~:<~-1i~@{~a~^~_ ~}~:>~)~%"
              (loop for i in slots collect (format nil "%:~a" i)))
      (format t "                               ,pointer (:struct ~(%:~a~)))~%"
              name)
      (format t "       (setf ")
      (let ((fixed-strings nil))
        (loop with first = t
              for m across (member struct)
              for n = (name m)
              do
                 (cond
                   ;; handle `type` specially
                   ((eql n 'type)
                    (setf first nil)
                    (format t "~(%:~a~) ~(:~a~)" n (make-const-keyword (values m))))
                   ;; handle members with fixed-length strings
                   ((and (eql (type m) :char)
                         (position #\[ (text m))
                         (not (position #\* (text m))))
                    (push m fixed-strings))
                   ;; normal members
                   (t
                    (unless first
                      (format t "~%             "))
                    (setf first nil)
                    (print-set n))))
        (format t ")~%")
        (loop for m in (reverse fixed-strings)
              do (let* ((.size (extract-array-size (text m)))
                        (size (format nil "%:~a"
                                      (if (get-constant .size)
                                          (make-const-symbol .size :add-++ t)
                                          (numeric-value .size))))
                        (.n (name m))
                        (n (format nil ",~a" .n))
                        (%n (format nil "%:~a" .n))
                        (sn (format nil "%:~a" (name struct)))
                        (p (format nil ",~a" (gethash .n -p))))
                   (assert size)
                   (format t "~&       ~(~a~)~%"
                           `(if ,(if (gethash .n -p)
                                     ;; if we have a -p argument,
                                     ;; treat explicit NIL as null
                                     ;; pointer, otherwise ""
                                     `(and ,p (not ,n))
                                     ;; if we have a default (so no
                                     ;; -p), always treat NIL as null
                                     ;; pointer, since default should
                                     ;; be "" if that was desired
                                     `(not ,n))
                                (setf ,%n "(cffi:null-pointer)")
                                ("cffi:lisp-string-to-foreign"
                                 (or ,n "\"\"")
                                 ("cffi:foreign-slot-pointer"
                                  ",pointer" '(":struct" ,sn)
                                  ',%n)
                                 ,size
                                 ":encoding :utf-8"))))))
      (format t "       ,@(if %slots body nil))~%")
      (format t "     ,@(if %slots nil body)))~%"))
    w))

#++
(ql:quickload '3b-openxr-generator)
;;;;; run this to regenerate bindings
#++
(progn
  (clrhash *exports*)
  (setf *constants-to-print* nil)
  (collect-enums *spec*)
  (collect-structs *spec*)
  (collect-command-definers *spec*)

  (alexandria:with-output-to-file (*standard-output*
                                   (asdf:system-relative-pathname
                                    :3b-openxr "bindings.lisp")
                                   :if-exists :supersede)
    (format t "(in-package #:~a)~%" *bindings-package-name*)
    (let ((*print-constants* nil))
      (print-comment *spec*)
      (print-types *spec*)
      (print-commands *spec*)))

  (alexandria:with-output-to-file (*standard-output*
                                   (asdf:system-relative-pathname
                                    :3b-openxr "constants.lisp")
                                   :if-exists :supersede)
    (format t "(in-package #:~a)~%" *bindings-package-name*)
    (format t ";; API Constants")
    (let ((ac (car (gethash 'api-constants *enum-types*))))
      (print-comment ac)
      (loop for e across (enum ac)
            do (print-comment e)
               (when (cl:member (name e) '(:true :false))
                 (format t "~&(defconstant ~(~a~) ~a)" (name e) (value e)))
               (print-constant e :mark "+")))
    (format t "~%~%;; #define")
    (loop for i in (reverse *constants-to-print*)
          do (print-define i))
    (format t "~%~%;; misc globals")
    (loop for e in (reverse (cdr (gethash :globals *enum-types*)))
          do (print-constant e :mark (if (position (raw-name e)
                                                   *named-constants*
                                                   :test 'string=)
                                         "+" ""))))

  (alexandria:with-output-to-file (*standard-output*
                                   (asdf:system-relative-pathname
                                    :3b-openxr "bindings-package.lisp")
                                   :if-exists :supersede)
    (format t "(defpackage #:~a~%" *bindings-package-name*)
    (format t "  (:use :cl #:cffi)~%")
    (format t "  (:shadow ~(~{#:~a~^~%           ~}~))~%"
            (sort (flet ((x (s)
                           (or (eql :external
                                    (nth-value 1 (find-symbol s :cl)))
                               (eql :external
                                    (nth-value 1 (find-symbol s :cffi))))))
                    (loop for i in (alexandria:hash-table-keys *exports*)
                          for s = (string-upcase i)
                          when (x s)
                            collect s))
                  'string<))
    (format t "  (:export ~(~{#:~a~^~%           ~}~)))~%"
            (sort (alexandria:hash-table-keys *exports*)
                  'string< :key 'string-downcase))
    (format t "(in-package #:~a)~%" *bindings-package-name*)
    (format t ";; internals used by extension function wrapper generator~%")
    (format t "(defconstant %extension-function-count% ~a)~%"
            *ext-fun-index*)
    (format t "(deftype %extension-function-vector% () `(simple-array function (~a)))~%"
            *ext-fun-index*))

  (a:with-output-to-file (*standard-output*
                          (asdf:system-relative-pathname
                           :3b-openxr "generated-macros.lisp")
                          :if-exists :supersede)
    (format t "(in-package #:~a)~%" *mid-level-package-name*)
    (format t ";; used by with-two-call~%")
    (format t "(defparameter %struct-types% (make-hash-table))~%")
    (loop
      for i from 0
      for s across (get-types *spec* 'type/struct)
      ;; todo: generate for alias structs too
      for name = (name s)
      unless (gethash name *skip-struct*)
        do (let ((w (print-with-struct s :name name)))
             (declare (ignorable w))
             (when w
               (loop
                 for m across (member s)
                 when (and (eql (name m) 'type)
                           (slot-boundp m 'values))
                   do (format t "(setf (gethash '~(%:~a~) %struct-types%)~%      '~(:~a~))~%"
                              name (make-enum-name (values m) nil))))))))
