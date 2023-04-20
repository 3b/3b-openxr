(in-package #:3b-openxr-wrappers)

(defvar *instance* nil)
(defvar *create-verbose* nil)

;;; mostly just for documentation, since defpackage should have
;;; corresponding import/export.
(defmacro import-export (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (dolist (sym ',symbols)
       (assert (eql (find-symbol (symbol-name sym)
                                 (find-package '#:3b-openxr))
                    sym)))))

(defun xr-error (result format &rest args)
  (let ((e (gethash result *error-conditions* 'simple-error)))
    (error e :format-control format :format-arguments args)))

(defmacro ignore-xr-error ((&rest error-types) &body body)
  "like IGNORE-ERRORS, but only ignore specific error types."
  `(handler-case (progn ,@body)
     ,@ (loop for e in error-types
              collect `(,e (condition) (values nil condition)))))

(defmacro with-foreign-string-array ((pointer strings) &body body)
  (a:with-gensyms (count i)
    (a:once-only (strings)
      `(let ((,count (length ,strings)))
         (cffi:with-foreign-object (,pointer :pointer ,count)
           (unwind-protect
                (progn
                  (loop for ,i below ,count
                        do (setf (cffi:mem-aref ,pointer :pointer ,i)
                                 (cffi:foreign-string-alloc
                                  (elt ,strings ,i)
                                  :encoding :utf-8)))
                  ,@body)
             (loop for ,i below ,count
                   do (cffi:foreign-string-free
                       (shiftf (cffi:mem-aref ,pointer :pointer ,i)
                               (cffi:null-pointer))))))))))

(cffi:defcallback debug-messenger-callback %::bool-32
    ((severity %::debug-utils-message-severity-flags-ext)
     (types %::debug-utils-message-type-flags-ext)
     (data (:pointer (:struct %:debug-utils-messenger-callback-data-ext)))
     (user-data :pointer))
  (assert (not (cffi:null-pointer-p data)))
  (cffi:with-foreign-slots ((%::type
                             %::next
                             %::message-id
                             %::function-name
                             %::message
                             %::object-count
                             %::objects
                             %::session-label-count
                             %::session-labels)
                            data (:struct %:debug-utils-messenger-callback-data-ext))
    (assert (eql %::type :type-debug-utils-messenger-callback-data-ext))
    ;; todo: dispatch to user callbacks? (by ID in user data or something?)
    user-data
    (flet ((ss (s) (if (and (symbolp s)
                            (a:ends-with-subseq "-EXT" (symbol-name s)))
                       (subseq (symbol-name s) 0 (- (length (symbol-name s)) 4))
                       s)))
      (format *debug-io* "~@[<~a> : ~]~(~{~a~^,~} : ~{~a~^,~} : ~)~a~%  ~
  ~a : ~a~%"
              (unless (cffi:null-pointer-p user-data)
                (cffi:pointer-address user-data))
              (mapcar #'ss (cffi:foreign-bitfield-symbols
                            '%::debug-utils-message-severity-flags-ext
                            severity))
              (mapcar #'ss (cffi:foreign-bitfield-symbols
                            '%::debug-utils-message-type-flags-ext
                            types))
              %::message-id %:function-name
              %::message)
      (when (plusp %:session-label-count)
        (format t " @")
        (loop for i below %:session-label-count
              for info = (cffi:mem-aref
                          %:session-labels
                          '(:struct %:debug-utils-label-ext)
                          i)
              do (format t " > ~a" (getf info '%:label-name)))
        (format t "~%"))

      (when (plusp %:object-count)
        (if (= 1 %:object-count)
            (format t "  object: ")
            (format t "  ~s objects:~%" %:object-count))
        (loop for i below %:object-count
              for info = (cffi:mem-aref
                          %:objects
                          '(:struct %:debug-utils-object-name-info-ext)
                          i)
              do (when (> %:object-count 1)
                   (format t "   "))
                 (format t "~x (~a), type ~(~a~)~%"
                         (getf info '%:object-handle)
                         (getf info '%:object-name)
                         (getf info '%:object-type)))))
    0))
