(in-package #:3b-openxr-wrappers)

;;; 3. API Initialization

;; 3.1. Exported Functions

;; 3.2. Function Pointers

(defun get-instance-proc-addr (name &key (instance *instance*))
  (cffi:with-foreign-object (p :pointer)
    (let ((r (%:get-instance-proc-addr (or instance 0) name p)))
      (if (unqualified-success r)
          (cffi:mem-ref p :pointer)
          r))))
