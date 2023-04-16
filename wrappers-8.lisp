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
    (list %:view-configuration-type :mutable (not (zerop %:fov-mutable)))))

(defun enumerate-view-configuration-views (system-id view-configuration-type)
  (with-two-call (i o p (:struct %:view-configuration-view))
    (%:enumerate-view-configuration-views
     *instance* system-id view-configuration-type i o p)))
