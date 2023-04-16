(in-package #:3b-openxr-wrappers)

;;; 5. System

;; 5.1. Form Factors

;; 5.2. Getting the XrSystemId

(defun get-system (form-factor)
  (cffi:with-foreign-objects ((sgi '(:struct %:system-get-info))
                              (p :pointer))
    (cffi:with-foreign-slots ((%:type
                               %:next
                               %:form-factor)
                              sgi (:struct %::system-get-info))
      (setf %:type :type-system-get-info
            %:next (cffi:null-pointer)
            %:form-factor form-factor))
    (check-result (%:get-system *instance* sgi p))
    (cffi:mem-ref p '%:system-id)))

;; 5.3. System Properties

(defun get-system-properties (system-id)
  ;; todo: ether add option for structs to add to .next, or detect
  ;; them from enabled extension in instance
  (m:with-system-properties (sp :%slots t)
    (check-result (%:get-system-properties *instance* system-id sp))
    (list :system-id %:system-id
          :vendor-id %:vendor-id
          :system-name (cffi:foreign-string-to-lisp
                        %:system-name :max-chars %:+max-system-name-size+
                        :encoding :utf-8)
          ;; todo: translate these more? (keywords instead of %:foo)
          :graphics-properties %:graphics-properties
          :tracking-properties %:tracking-properties)))
