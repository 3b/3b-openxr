(in-package #:3b-openxr-wrappers)

;; with-foo and other macros

(defmacro with-returned-atom ((var type) &body form)
  ;; &body just for indentation
  (assert (= 1 (length form)))
  (setf form (car form))
  `(cffi:with-foreign-object (,var ',type)
     (check-result ,form)
     (cffi:mem-ref ,var ',type)))

(defmacro with-returned-handle ((var type object-type &key name) &body form)
  ;; &body just for indentation
  (assert (= 1 (length form)))
  (setf form (car form))
  `(cffi:with-foreign-object (,var ',type)
     (check-result ,form)
     (when *create-verbose*
       (format *debug-io* "created ~s = #x~x~%" ',type (cffi:mem-ref ,var ',type)))
     ,(if object-type
          `(wrap-handle (cffi:mem-ref ,var ',type) ,object-type ,name)
          ;; for things that wrap manually
          `(cffi:mem-ref ,var ',type))))

(defmacro with-instance ((&rest r
                          &key
                            ;; instance-create-info
                            create-flags extensions layers
                            ;; application-info
                            (application-name "3b-OpenXR Application")
                            (application-version 1)
                            (engine-name "3b-OpenXR/No Engine")
                            (engine-version 0)
                            (api-version %::+current-api-version+)
                            ;; debug-utils-messenger-create-info-ext
                            message-severities message-types user-callback
                            (user-data 0)
                            ;; todo: instance-create-info-android?
                            ;; next pointer?
                            )
                         &body body)
  (declare (ignore create-flags extensions layers
                   application-name application-version
                   engine-name engine-version api-version
                   message-severities message-types user-callback
                   user-data object-name))
  `(let ((*instance* (create-instance ,@r)))
     (unwind-protect
          (progn
            ,@body)
       (when *create-verbose*
         (format *debug-io* "~&destroy instance #x~x~%" *instance*))
       (destroy-instance *instance*))))

(defmacro with-debug-messsenger ((var &rest r
                                  &key message-severities message-types
                                    user-callback (user-data 0))
                                 &body body)
  (declare (ignore message-types message-severities user-callback user-data))
  `(let ((,var (create-debug-utils-messenger-ext ,@r)))
     (unwind-protect (progn ,@body)
       (when *create-verbose*
         (format *debug-io* "~&destroy messenger #x~x~%" ,var))
       (%:destroy-debug-utils-messenger-ext ,var))))



