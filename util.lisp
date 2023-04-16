(in-package #:3b-openxr-wrappers)

(defvar *instance* nil)

;;; mostly just for documentation, since defpackage should have
;;; corresponding import/export.
(defmacro import-export (&rest symbols)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (dolist (sym ',symbols)
       (assert (eql (find-symbol (symbol-name sym)
                                 (find-package '#:3b-openxr))
                    sym)))))


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
    (format *debug-io* "~@[<~a:>~]~s:~s:~s:~s:~s~%"
            (unless (cffi:null-pointer-p user-data)
              (cffi:pointer-address user-data))
            severity types
            %::message-id %::function-name %::message)
    ;; todo: handle objects and labels
    0))
