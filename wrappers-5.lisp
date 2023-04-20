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
    (check-result (%:get-system (handle *instance*) sgi p))
    (cffi:mem-ref p '%:system-id)))

;; 5.3. System Properties

(defun %get-system-properties (system-id &key (next (cffi:null-pointer)))
  (m:with-system-properties (sp :%slots t
                                :next next)
    (check-result (%:get-system-properties (handle *instance*) system-id sp))
    (list :system-id %:system-id
          :vendor-id %:vendor-id
          :system-name (cffi:foreign-string-to-lisp
                        %:system-name :max-chars %:+max-system-name-size+
                        :encoding :utf-8)
          :max-swapchain-image-width (getf %:graphics-properties
                                           '%:max-swapchain-image-width)
          :max-swapchain-image-height (getf %:graphics-properties
                                            '%:max-swapchain-image-height)
          :max-layer-count (getf %:graphics-properties '%:max-layer-count)
          :position-tracking (not (zerop (getf %:tracking-properties
                                               '%:position-tracking)))
          :orientation-tracking (not (zerop (getf %:tracking-properties
                                                  '%:orientation-tracking))))))

(defun get-system-properties (system-id)
  (%get-system-properties system-id))

(defun get-system-hand-tracking-properties-ext (system-id)
  (m:with-system-hand-tracking-properties-ext (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-hand-tracking (not (zerop %:supports-hand-tracking))
             r))))

(defun get-system-eye-gaze-interaction-properties-ext (system-id)
  (m:with-system-eye-gaze-interaction-properties-ext (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-eye-gaze-interaction
             (not (zerop %:supports-eye-gaze-interaction))
             r))))

(defun get-system-face-tracking-properties-fb (system-id)
  (m:with-system-face-tracking-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-face-tracking (not (zerop %:supports-face-tracking))
             r))))

(defun get-system-body-tracking-properties-fb (system-id)
  (m:with-system-body-tracking-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-body-tracking (not (zerop %:supports-body-tracking))
             r))))

(defun get-system-eye-tracking-properties-fb (system-id)
  (m:with-system-eye-tracking-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-eye-tracking (not (zerop %:supports-eye-tracking)) r))))

(defun get-system-hand-tracking-mesh-properties-msft (system-id)
  (m:with-system-hand-tracking-mesh-properties-msft (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-hand-tracking-mesh (not (zerop
                                                %:supports-hand-tracking-mesh))
             :max-hand-mesh-index-count %:max-hand-mesh-index-count
             :max-hand-mesh-vertex-count %:max-hand-mesh-vertex-count
             r))))

(defun get-system-color-space-properties-fb (system-id)
  (m:with-system-color-space-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :color-space %:color-space r))))

(defun get-system-spatial-entity-properties-fb (system-id)
  (m:with-system-spatial-entity-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-spatial-entity (not (zerop %:supports-spatial-entity))
             r))))

(defun get-system-foveation-eye-tracked-properties-meta (system-id)
  (m:with-system-foveation-eye-tracked-properties-meta (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-foveation-eye-tracked
             (not (zerop %:supports-foveation-eye-tracked))
             r))))

(defun get-system-render-model-properties-fb (system-id)
  (m:with-system-render-model-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-render-model-loading
             (not (zerop %:supports-render-model-loading))
             r))))

(defun get-system-keyboard-tracking-properties-fb (system-id)
  (m:with-system-keyboard-tracking-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-keyboard-tracking
             (not (zerop %:supports-keyboard-tracking))
             r))))

(defun get-system-foveated-rendering-properties-varjo (system-id)
  (m:with-system-foveated-rendering-properties-varjo (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-foveated-rendering
             (not (zerop %:supports-foveated-rendering))
             r))))

(defun get-system-passthrough-properties-fb (system-id)
  (m:with-system-passthrough-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-passthrough (not (zerop %:supports-passthrough))
             r))))

(defun get-system-passthrough-properties-2-fb (system-id)
  (m:with-system-passthrough-properties-2-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :capabilities %:capabilities r))))

(defun get-system-facial-tracking-properties-htc (system-id)
  (m:with-system-facial-tracking-properties-htc (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :support-eye-facial-tracking
             (not (zerop %:support-eye-facial-tracking))
             :support-lip-facial-tracking
             (not (zerop %:support-lip-facial-tracking))
             r))))

(defun get-system-space-warp-properties-fb (system-id)
  (m:with-system-space-warp-properties-fb (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :recommended-motion-vector-image-rect-width
             %:recommended-motion-vector-image-rect-width
             :recommended-motion-vector-image-rect-height
             %:recommended-motion-vector-image-rect-height
             r))))

(defun get-system-marker-tracking-properties-varjo (system-id)
  (m:with-system-marker-tracking-properties-varjo (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-marker-tracking (not (zerop %:supports-marker-tracking))
             r))))

(defun get-system-headset-id-properties-meta (system-id)
  (m:with-system-headset-id-properties-meta (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :id %:id r))))

(defun get-system-force-feedback-curl-properties-mndx (system-id)
  (m:with-system-force-feedback-curl-properties-mndx (h :%slots t)
    (let ((r (%get-system-properties system-id :next h)))
      (list* :supports-force-feedback-curl
             (not (zerop %:supports-force-feedback-curl))
             r))))
