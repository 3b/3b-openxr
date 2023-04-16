(in-package #:3b-openxr-bindings)

;; hand-written wrappers for some simple struct types
(defmacro define-cffi-translators ((pointer-var value-var type class)
                                   &body body &key read write)
  (declare (ignore body))
  `(progn
     ,@(when write
         `((defmethod cffi::expand-to-foreign-dyn-indirect (,value-var
                                                            ,pointer-var
                                                            body
                                                            (type ,class))
             `(with-foreign-object (,,pointer-var ',',type)
                ,,write
                ,@body))
           (macrolet ((w (,value-var ,pointer-var)
                        ,write))
             (defmethod translate-into-foreign-memory (,value-var (type ,class)
                                                       ,pointer-var)
               (w ,value-var ,pointer-var))
             (defmethod translate-to-foreign (,value-var (type ,class))
               (let ((,pointer-var (foreign-alloc ',type)))
                 (w ,value-var ,pointer-var)
                 ,pointer-var)))))
     ,@(when read
         `((defmethod expand-from-foreign (,pointer-var (type ,class))
             (alexandria:once-only (,pointer-var) ,read))
           (macrolet ((r (,pointer-var)
                        ,read))
             (defmethod translate-from-foreign (,pointer-var (type ,class))
               (r ,pointer-var)))))))

(define-cffi-translators (p v (:struct vector-2f) vector-2f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (2))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float))))
  :read `(make-array '(2)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1))))

(define-cffi-translators (p v (:struct vector-3f) vector-3f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (3))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)
                   (cffi:mem-aref ,p :float 2) (aref ,v 2)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float)
                   (cffi:mem-aref ,p :float 2) (coerce (elt ,v 2) 'single-float))))
  :read `(make-array '(3)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1)
                                             (cffi:mem-aref ,p :float 2))))

(define-cffi-translators (p v (:struct vector-4f) vector-4f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (4))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)
                   (cffi:mem-aref ,p :float 2) (aref ,v 2)
                   (cffi:mem-aref ,p :float 3) (aref ,v 3)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float)
                   (cffi:mem-aref ,p :float 2) (coerce (elt ,v 2) 'single-float)
                   (cffi:mem-aref ,p :float 3) (coerce (elt ,v 3) 'single-float))))
  :read `(make-array '(4)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1)
                                             (cffi:mem-aref ,p :float 2)
                                             (cffi:mem-aref ,p :float 3))))

(define-cffi-translators (p v (:struct color-4f) color-4f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (4))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)
                   (cffi:mem-aref ,p :float 2) (aref ,v 2)
                   (cffi:mem-aref ,p :float 3) (aref ,v 3)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float)
                   (cffi:mem-aref ,p :float 2) (coerce (elt ,v 2) 'single-float)
                   (cffi:mem-aref ,p :float 3) (coerce (elt ,v 3) 'single-float))))
  :read `(make-array '(4)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1)
                                             (cffi:mem-aref ,p :float 2)
                                             (cffi:mem-aref ,p :float 3))))

(define-cffi-translators (p v (:struct quaternion-f) quaternion-f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (4))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)
                   (cffi:mem-aref ,p :float 2) (aref ,v 2)
                   (cffi:mem-aref ,p :float 3) (aref ,v 3)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float)
                   (cffi:mem-aref ,p :float 2) (coerce (elt ,v 2) 'single-float)
                   (cffi:mem-aref ,p :float 3) (coerce (elt ,v 3) 'single-float))))
  :read `(make-array '(4)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1)
                                             (cffi:mem-aref ,p :float 2)
                                             (cffi:mem-aref ,p :float 3))))


(define-cffi-translators (p v (:struct offset-2d-f) offset-2d-f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (2))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float))))
  :read `(make-array '(2)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1))))

(define-cffi-translators (p v (:struct extent-2d-f) extent-2d-f-tclass)
  :write `(etypecase ,v
            ((simple-array single-float (2))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (coerce (elt ,v 0) 'single-float)
                   (cffi:mem-aref ,p :float 1) (coerce (elt ,v 1) 'single-float))))
  :read `(make-array '(2)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1))))


(define-cffi-translators (p v (:struct extent-2d-i) extent-2d-i-tclass)
  :write `(etypecase ,v
            ((simple-array (signed-byte 32)(2))
             (setf (cffi:mem-aref ,p :int32 0) (aref ,v 0)
                   (cffi:mem-aref ,p :int32 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :int32 0) (elt ,v 0)
                   (cffi:mem-aref ,p :int32 1) (elt ,v 1))))
  :read `(make-array '(2)
                     :element-type '(signed-byte 32)
                     :initial-contents (list (cffi:mem-aref ,p :int 0)
                                             (cffi:mem-aref ,p :int 1))))

(define-cffi-translators (p v (:struct offset-2d-i) offset-2d-i-tclass)
  :write `(etypecase ,v
            ((simple-array (signed-byte 32)(2))
             (setf (cffi:mem-aref ,p :int32 0) (aref ,v 0)
                   (cffi:mem-aref ,p :int32 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :int32 0) (elt ,v 0)
                   (cffi:mem-aref ,p :int32 1) (elt ,v 1))))
  :read `(make-array '(2)
                     :element-type '(signed-byte 32)
                     :initial-contents (list (cffi:mem-aref ,p :int 0)
                                             (cffi:mem-aref ,p :int 1))))

