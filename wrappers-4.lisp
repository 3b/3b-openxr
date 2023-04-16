(in-package #:3b-openxr-wrappers)

;;; 4. Instance

;; 4.1. API Layers and Extensions

;; fixme: should these return a real data structure?

(defun enumerate-api-layer-properties ()
  (with-two-call (i o p (:struct %:api-layer-properties)
                  :filter (lambda (a)
                            (list
                             :name (cffi:foreign-string-to-lisp
                                    (getf a '%:layer-name)
                                    :max-chars %::+max-api-layer-name-size+)
                             :version (getf a '%:layer-version)
                             :spec-version (getf a '%:spec-version)
                             :description (cffi:foreign-string-to-lisp
                                           (getf a '%:description)
                                           :max-chars %::+max-api-layer-description-size+))))
    (%:enumerate-api-layer-properties i o p)))



(defun enumerate-instance-extension-properties (layer)
  (with-two-call (i o p (:struct %:extension-properties)
                  :filter (lambda (a)
                            (list
                             :name (cffi:foreign-string-to-lisp
                                    (getf a '%::extension-name)
                                    :max-chars %::+max-extension-name-size+)
                             :version (getf a '%::extension-version))))
    (%:enumerate-instance-extension-properties (or layer (cffi:null-pointer))
                                               i o p)))

;; 4.2. Instance Lifecycle

(defun create-instance (&key
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
  (with-debug-utils-messenger-create-info-ext
      (dumci :message-severities message-severities
             :message-types message-types
             :user-callback user-callback
             :user-data user-data)
    (with-application-info (ai :application-version application-version
                               :engine-version engine-version
                               :api-version api-version
                               :application-name application-name
                               :engine-name engine-name)
      (with-foreign-string-array (fs-extensions extensions)
        (with-foreign-string-array (fs-layers layers)
          (with-instance-create-info (ici
                                      :next (if user-callback
                                                dumci
                                                ;; todo: generate casts for next?
                                                (cffi:null-pointer))
                                      :create-flags create-flags
                                      :application-info ai
                                      :enabled-api-layer-count (length layers)
                                      :enabled-api-layer-names fs-layers
                                      :enabled-extension-count (length extensions)
                                      :enabled-extension-names fs-extensions)
            (cffi:with-foreign-object (instance '%::instance)
              (let ((r (%::create-instance ici instance)))
                (unless (unqualified-success r)
                  (error "create instance failed ~s?" r))
                (format t "~&created instance ~x~%" (cffi:mem-ref instance '%::instance))
                (cffi:mem-ref instance '%::instance)))))))))


(import-export %:destroy-instance)

;; 4.3. Instance Information

(defun get-instance-properties ()
  (m:with-instance-properties (ip :%slots t)
    (check-result (%:get-instance-properties *instance* ip))
    (list :runtime-name (cffi:foreign-string-to-lisp
                         %:runtime-name :max-chars %:+max-runtime-name-size+
                         :encoding :utf-8)
          :version (list (%:version-major %:runtime-version)
                         (%:version-minor %:runtime-version)
                         (%:version-patch %:runtime-version)))))

;; 4.4. Platform-Specific Instance Creation

;; 4.5. Instance Enumerated Type String Functions

;; not sure how useful these are, since we have cl keywords
;;
;; todo: if result changes to ints, add result-to-keyword

(defun result-to-string (result)
  (cffi:with-foreign-pointer-as-string (p %:+max-result-string-size+
                                          :encoding :utf-8
                                          :max-chars %:+max-result-string-size+)
    (check-result (%:result-to-string (or *instance* 0) result p))))

(defun structure-type-to-string (type)
  (cffi:with-foreign-pointer-as-string (p %:+max-structure-name-size+
                                          :encoding :utf-8
                                          :max-chars %:+max-structure-name-size+)
    (check-result (%:structure-type-to-string (or *instance* 0) type p))))
