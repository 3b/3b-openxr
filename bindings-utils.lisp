(in-package #:3b-openxr-bindings)

;; misc things needed to evaluate the generated bindings, including
;; extension handling

;;; todo: add some option for user-defined extension functions?
;; fixed size array makes it hard for users to add functions, so for
;; now they have to call get-proc-address and call the pointer
;; manually. Maybe add some hash-based version of DEFEXTFUN so they
;; can define them similarly to the generated bindings at a slight
;; performance penalty?

;;; todo: decide if the vector stuff is actually faster
;; calling through a vector w/fixed indices is expected to be faster
;; than a hash or whatever, but not actually tested, so try some
;; alternatives? hash would be more flexible if users needed to define
;; their own functions (for extensions not in spec, etc) for example

;;; todo: add option to just generate global extension functions?
;; assuming it is faster, add an option to generate cl-opengl style
;; global extension functions that redefine themselves or whatever,
;; for applications that know they will only ever use one instance

(defvar *function-index* (make-hash-table))

(defvar *function-getters*
  ;; functions to look up the current extension. used to initialize
  ;; %INSTANCE-POINTERS, and expected to update corresponding slot of
  ;; %INSTANCE-POINTERS of current *INSTANCE* when called, and call
  ;; the new value. (New value should be a function to either call
  ;; specified extension function or error if not found. Callers that
  ;; need to determine if an extension exists without erroring should
  ;; call get-instance-proc-addr directly.
  (make-array %extension-function-count%
              :element-type 'function
              :initial-element (lambda (&rest r)
                                 (declare (ignore r))
                                 (error "openxr extension support not initialized correctly?"))))

;; wrapper for instance, containing handle and function pointers
(defstruct %instance
  ;; 0 is +null-handle+, but that isn't defined yet
  (handle 0 :type (unsigned-byte 64))
  (pointers *function-getters* :type %extension-function-vector%))

(defun wrap-instance (handle)
  (make-%instance :handle (or handle 0)
                  :pointers (copy-seq *function-getters*)))

(declaim (type %instance *instance*))
;; global value is NULL instance, which only has a few valid functions
;; that can be queried (not handled specially here, since list could
;; change with extensions and extensions are expected to return NULL
;; for anything not on the list)
(defvar *instance* (wrap-instance nil))
#+sbcl(declaim (sb-ext:always-bound *instance*))



(defmacro with-instance ((handle) &body body)
  `(let ((*instance* (wrap-instance ,handle)))
     ,@body))

(defmacro defextfun ((foreign-name lisp-name index) result-type &rest body)
  (let ((args-list (mapcar #'first body)))
    (alexandria:with-gensyms (pointer)
      `(progn
         (setf (aref *function-getters* ,index)
               (lambda (&rest r)
                 (let* ((handle (%instance-handle *instance*))
                        (h (%instance-pointers *instance*)))
                   (cffi:with-foreign-object (p '(:pointer pfn-void-function))
                     (let ((ret (get-instance-proc-addr handle ,foreign-name
                                                        p)))
                       (if (= ret success)
                           (setf (aref h ,index)
                                 (let ((,pointer (cffi:mem-ref p :pointer)))
                                   (lambda (,@args-list)
                                     (foreign-funcall-pointer
                                      ,pointer
                                      (:library openxr-loader)
                                      ,@(loop for i in body
                                              collect (second i)
                                              collect (first i))
                                      ,result-type))))
                           (setf (aref h ,index)
                                 (lambda (&rest r)
                                   (declare (ignore r))
                                   (error "extension function ~s not found:~s~%(~s)"
                                          ',lisp-name ret ',foreign-name))))))
                   (apply (aref h ,index) r))))
         (declaim (inline ,lisp-name))
         (defun ,lisp-name ,args-list
           (funcall (aref (%instance-pointers *instance*) ,index)
                    ,@args-list))
         ',lisp-name))))


;;; replacement for cffi defbitfield that doesn't automatically
;;; translate to/from symbols. instead, use the integer value
;;; directly, and generate functions to use them (with compiler
;;; macros)

(define-foreign-type bitfield* (cffi::foreign-bitfield)
  ())

(defun make-bitfield* (type-name base-type values)
  "Makes a new instance of the bitfield* class."
  (multiple-value-bind (base-type keyword-values value-keywords
                        field-keywords bit-index->keyword)
      (cffi::parse-foreign-enum-like type-name base-type values t)
    (make-instance 'bitfield*
                   :name type-name
                   :actual-type (cffi::parse-type base-type)
                   :keyword-values keyword-values
                   :value-keywords value-keywords
                   :field-keywords field-keywords
                   :bit-index->keyword bit-index->keyword)))

(defun define-bitfield*-accessors (name)
  ;; assume %defcenum-like created type already
  (let ((type (cffi::parse-type name))
        (cl-type `(unsigned-byte ,(cffi:foreign-type-size name))))

    `((defun ,name (&rest flags)
        (loop with r of-type ,cl-type = 0
              for i in flags
              for b = (gethash i (cffi::keyword-values ,type))
              do (assert b () "Unknown keyword ~s in bitfield* type ~s?" i)
                 (setf r (logior r b))
              finally (return r)))
      (define-compiler-macro ,name (&whole w &rest flags)
        (if (every #'constantp flags)
            (apply #',name flags)
            w)))))

(defmacro %defbitfield*-accessors (name)
  ;; expand in 2 steps so we can use the results of the cffi
  ;; definition of the type to define the accessors
  `(progn ,@(define-bitfield*-accessors name)))

(defmacro defbitfield* (name-and-options &body masks)
  (let ((name (if (consp name-and-options)
                  (car name-and-options)
                  name-and-options)))
    `(progn
       ,(cffi::%defcenum-like name-and-options masks 'make-bitfield*)
       (%defbitfield*-accessors ,name))))

(defmethod translate-to-foreign (value (type bitfield*))
  ;; might as well accept keyword list
  (if (integerp value)
      value
      (cffi::%foreign-bitfield-value type (alexandria:ensure-list value))))

(defmethod translate-from-foreign (value (type bitfield*))
  ;; don't translate to keywords
  value)

(defmethod expand-to-foreign (value (type bitfield*))
  (flet ((expander (value type)
           `(if (integerp ,value)
                ,value
                (cffi::%foreign-bitfield-value ,type (alexandria:ensure-list ,value)))))
    (if (constantp value)
        (eval (expander value type))
        (expander value type))))

(defmethod expand-from-foreign (value (type bitfield*))
  value)

(defun bit-p (value type field)
  (logtest (cffi:foreign-bitfield-value type field) value))

(define-compiler-macro bit-p (&whole whole value type field)
  (if (and (constantp type) (constantp field))
      `(logtest (cffi:foreign-bitfield-value ,type ,field)
                ,value)
      whole))

(defmacro with-bits ((var type) &body body)
  (let ((original var))
    (alexandria:once-only (var)
      `(macrolet ((,original (bit)
                    `(bit-p ,',var ',',type ,bit)))
         ,@body))))
