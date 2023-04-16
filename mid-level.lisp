(in-package #:3b-openxr-mid-level)

;; functions called by generated macros
(defun cast-callback (n)
  (etypecase n
    (cffi:foreign-pointer n)
    (null (cffi:null-pointer))
    (symbol (cffi:get-callback n))))

(defun cast-int-pointer (n)
  (if (typep n 'unsigned-byte)
      (cffi:make-pointer n)
      n))
