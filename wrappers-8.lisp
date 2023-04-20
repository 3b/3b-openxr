(in-package #:3b-openxr-wrappers)

;;; 8. View Configurations

;; 8.1. Primary View Configurations

;; 8.2. View Configuration API

(defun enumerate-view-configurations (system-id)
  (with-two-call (i o p %:view-configuration-type)
    (%:enumerate-view-configurations *instance* system-id i o p)))

(defun get-view-configuration-properties (system-id view-configuration-type)
  (with-view-configuration-properties (vcp :%slots t)
    (check-result (%:get-view-configuration-properties *instance* system-id
                                                       view-configuration-type
                                                       vcp))
    (list %:view-configuration-type :fov-mutable (not (zerop %:fov-mutable)))))

(defun enumerate-view-configuration-views (system-id view-configuration-type)
  (with-two-call (i o p (:struct %:view-configuration-view)
                  :filter-pointer
                  (lambda (p)
                    (cffi:with-foreign-slots ((%:MAX-SWAPCHAIN-SAMPLE-COUNT
                                               %:RECOMMENDED-SWAPCHAIN-SAMPLE-COUNT
                                               %:MAX-IMAGE-RECT-HEIGHT
                                               %:RECOMMENDED-IMAGE-RECT-HEIGHT
                                               %:MAX-IMAGE-RECT-WIDTH
                                               %:RECOMMENDED-IMAGE-RECT-WIDTH
                                               %:NEXT
                                               %:TYPE)
                                              p
                                              (:struct %:view-configuration-view))
                      (declare (ignore %:type))
                      (list*
                       :RECOMMENDED-IMAGE-RECT-HEIGHT %:RECOMMENDED-IMAGE-RECT-HEIGHT
                       :RECOMMENDED-IMAGE-RECT-WIDTH %:RECOMMENDED-IMAGE-RECT-WIDTH
                       :RECOMMENDED-SWAPCHAIN-SAMPLE-COUNT %:RECOMMENDED-SWAPCHAIN-SAMPLE-COUNT
                       :MAX-IMAGE-RECT-HEIGHT %:MAX-IMAGE-RECT-HEIGHT
                       :MAX-IMAGE-RECT-WIDTH %:MAX-IMAGE-RECT-WIDTH
                       :MAX-SWAPCHAIN-SAMPLE-COUNT %:MAX-SWAPCHAIN-SAMPLE-COUNT
                       (unless (cffi:null-pointer-p %:NEXT)
                         :next (list :unparsed
                                     (structure-type-to-string
                                      (cffi:foreign-slot-value
                                       %:next '(:struct %:base-in-structure)
                                       '%:type))))))))
    (%:enumerate-view-configuration-views
     *instance* system-id view-configuration-type i o p)))
