(in-package #:3b-openxr-parse-spec2)

;; stuff that needs definitions from schema.lisp, and doesn't like to
;; be in same file

(defmethod initialize-instance :after ((o enum) &key)
  (when (and (slot-boundp o 'bitpos)
             (not (slot-boundp o 'value)))
    (setf (slot-value o 'value)
          (ash 1 (numeric-value (bitpos o))))))

(defmethod initialize-instance :after ((o enum/ext) &key)
  (let ((number (gethash :number (parent-parser-slots :depth 2)))
        (dir (if (slot-boundp o 'dir)
                 (dir o)
                 nil)))
    (cond
      ((a:ends-with-subseq "_SPEC_VERSION" (raw-name o))
       ;; todo: do something with these
       )
      ((a:ends-with-subseq "_EXTENSION_NAME" (raw-name o))
       ;; todo: do something with these
       )
      ((and (slot-boundp o 'extends) (slot-boundp o 'offset))
       (setf (slot-value o 'value) (ext-enum-value number dir (offset o))))
      ((and (slot-boundp o 'extends) (slot-boundp o 'bitpos))
       (setf (slot-value o 'value) (ash 1 (numeric-value (bitpos o)))))
      ((and (slot-boundp o 'value)
            (not (slot-boundp o 'extends))
            (not (slot-boundp o 'offset))))
      ((slot-boundp o 'alias)
       )
      (t
       (break "todo ~s" o)))))

;; add nicer names for some slots
(defmethod error-codes ((a has-errorcodes))
  (errorcodes a))
(defmethod success-codes ((a has-successcodes))
  (successcodes a))

(defmethod initialize-instance :after ((o has-name) &key)
  (when (slot-boundp o 'name)
    (let ((name (name o)))
      (setf (slot-value o 'raw-name) name)
      (setf (slot-value o 'name)
            (typecase o
              ((or tag)
               (push name *suffix-list*)
               name)
              (enum
               (make-enum-name name (get-current-enums-name)))
              (enum/ext
               (make-enum-name name (get-node-attrib 'extends)))
              (enums
               (%translate-type-name name))
              ((or param member)
               (translate-var-name name))
              (command
               (%translate-type-name name))
              (interaction-profile
               ;; not sure yet. looks like a path, so maybe split and
               ;; put into a tree at some point?
               name)
              (type/define
               (make-const-keyword name))
              ((or type/basetype
                   type/bitmask
                   type/handle
                   type/enum
                   type/struct
                   type/funcpointer
                   command/alias)
               (%translate-type-name name))


              ((or extension feature require vendorid type/include
                   type/requires)
               ;; don't show up in bindings for now, so no translation?
               name)
              (t
               (break "does ~s need names translated?~%~s" (type-of o)
                      name))))
      (format t "translated name from ~s to ~s (~s)~%"
              (raw-name o) (name o) (type-of o)))))

(defmethod initialize-instance :after ((o has-type) &key)
  (when (slot-boundp o 'type)
    (let ((type (type o)))
      (setf (slot-value o 'raw-type) type)
      (setf (slot-value o 'type)
            (typecase o
              ((or member param command)
               ;; normal types
               (%translate-type-name type))
              ((or component)
               ;;
               (assert (a:starts-with-subseq "XR_ACTION_TYPE" type))
               (make-enum-name type "XrActionType"))
              (enums
               (unless (or (string= type "enum")
                           (string= type "bitmask"))
                 (break "enum type ~s?" type))
               (%translate-type-name type))
              (require
               ;; vector of type and enum value names?
               #++(break "~s" (map 'vector (lambda (a)
                                             (or (gethash a *known-enum-names*)
                                                 (%translate-type-name a)))
                                   type))
               (map 'vector (lambda (a)
                              (or (gethash a *known-enum-names*)
                                  (%translate-type-name a)))
                    type))
              (extension/require
               ;; vector of type and enum value names?
               #++(break "e/r ~s" (map 'vector (lambda (a)
                                                 (or (gethash a *known-enum-names*)
                                                     (%translate-type-name a)))
                                       type))
               (map 'vector (lambda (a)
                              (or (gethash a *known-enum-names*)
                                  (%translate-type-name a)))
                    type))
              (extension
               (unless (string= type "instance")
                 (break "extension ~s" type))
               type)
              (type/define
               (make-const-keyword type))
              ((or type/basetype type/bitmask)
               (%translate-type-name type))
              (type/handle
               (assert (string= type "XR_DEFINE_HANDLE"))
               :define-handle)
              (type/funcpointer
               (map 'vector '%translate-type-name type))
              (t
               (break "does ~s need types translated? (~s)" (type-of o)
                      type)))))))

(defmethod initialize-instance :after ((o has-alias) &key)
  (when (slot-boundp o 'alias)
    (let ((alias (alias o)))
      (setf (slot-value o 'raw-alias) alias)
      (setf (slot-value o 'alias)
            (typecase o
              ((or type/struct command/alias)
               (%translate-type-name alias))
              (enum/ext
               (let ((enum (gethash alias *known-enum-names*)))
                 (assert enum)
                 enum))
              (t
               (break "does ~s need alias translated? (~s)" (type-of o)
                      alias)))))))

(defmethod initialize-instance :after ((o has-extends) &key)
  (when (slot-boundp o 'extends)
    (let ((extends (extends o)))
      (setf (slot-value o 'raw-extends) extends)
      (setf (slot-value o 'extends)
            (typecase o
              #++((or type/struct command/alias)
                  (%translate-type-name extends))
              (enum/ext
               (%translate-type-name extends))
              (t
               (break "does ~s need extends translated? (~s)" (type-of o)
                      extends)))))))

(defmethod initialize-instance :after ((o has-len) &key)
  (when (slot-boundp o 'len)
    (let ((len (len o)))
      (setf (slot-value o 'raw-len) len)
      (setf (slot-value o 'len)
            (typecase o
              ((or member param)
               (let ((c (split-sequence:split-sequence #\, len)))
                 (print
                  (map 'vector (lambda (a)
                                 (if (string= a "null-terminated")
                                     :null-terminated
                                     (translate-var-name a)))
                       c))))
              (t
               (break "does ~s need len translated?~%~s" (type-of o) len)))))))

(defmethod initialize-instance :after ((o has-successcodes) &key)
  (when (slot-boundp o 'successcodes)
    (let ((codes (success-codes o)))
      (setf (slot-value o 'raw-success-codes) codes)
      (setf (slot-value o 'successcodes)
            (typecase o
              (command
               (let ((c (split-sequence:split-sequence #\, codes)))
                 (print
                  (map 'vector (lambda (a)
                                 (or (gethash a *known-enum-names*)
                                     (error "unknown success code ~s?" a)))
                       c))))
              (t
               (break "does ~s need success-codes translated?~%~s" (type-of o)
                      codes)))))))

(defmethod initialize-instance :after ((o has-errorcodes) &key)
  (when (slot-boundp o 'errorcodes)
    (let ((codes (error-codes o)))
      (setf (slot-value o 'raw-error-codes) codes)
      (setf (slot-value o 'errorcodes)
            (typecase o
              (command
               (print
                (let ((c (split-sequence:split-sequence #\, codes)))
                  (map 'vector (lambda (a)
                                 (or (gethash a *known-enum-names*)
                                     (error "unknown error code ~s?" a)))
                       c))))
              (t
               (break "does ~s need error-codes translated?~%~s" (type-of o)
                      codes)))))))
