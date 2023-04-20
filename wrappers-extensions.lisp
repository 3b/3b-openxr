(in-package #:3b-openxr-wrappers)

;;; 12. List of Current Extensions


;;; 12.1. XR_KHR_android_create_instance
;; add .next to CREATE-INSTANCE


;;; 12.2. XR_KHR_android_surface_swapchain
#+todo
(defun create-swapchain-android-surface-khr ...)


;;; 12.3. XR_KHR_android_thread_settings
#+todo
(defun set-android-application-thread-khr ...)


;;; 12.4. XR_KHR_binding_modification
;; adds .next to SUGGEST-INTERACTION-PROFILE-BINDINGS


;;; 12.5. XR_KHR_composition_layer_color_scale_bias
;; adds .next to COMPOSITION-LAYER-BASE-HEADER

;;; 12.6. XR_KHR_composition_layer_cube
;;; 12.7. XR_KHR_composition_layer_cylinder
;; add new composition layer struct types for end-frame

;;; 12.8. XR_KHR_composition_layer_depth
;; adds .next to COMPOSITION-LAYER-PROJECTION-VIEW


;;; 12.9. XR_KHR_composition_layer_equirect
;;; 12.10. XR_KHR_composition_layer_equirect2
;; add new composition layer struct types for end-frame

;;; 12.11. XR_KHR_convert_timespec_time
(defun convert-timespec-time-to-time-khr (timespec)
  (error "todo: decide how timespec should be translated")
  #++
  (cffi:with-foreign-object (p '%:time)
    (cffi:with-foreign-object (ts '(:struct timespec))
      (setf (cffi:mem-ref ts '(:struct timespec)) timespec)
      (%:convert-timespec-time-to-time-khr *instance* timespec p))
    (cffi:mem-ref p '%:time)))

(defun convert-time-to-timespec-time-khr (time)
  (error "todo: decide how timespec should be translated")
  #++
  (cffi:with-foreign-object (ts '(:struct timespec))
    (%:convert-time-to-timespec-time-khr *instance* time tx)
    (cffi:mem-ref ts '(:struct timespec))))

;;; 12.12. XR_KHR_D3D11_enable
#+todo(defun get-d3d11graphics-requirements-khr ...)
;; also handle graphics requirements, creating sessions and enumerating images

;;; 12.13. XR_KHR_D3D12_enable
#+todo(defun get-d3d12graphics-requirements-khr ...)
;; also handle graphics requirements, creating sessions and enumerating images

;;; 12.14. XR_KHR_loader_init
#+todo (defun initialize-loader-khr ...)

;;; 12.15. XR_KHR_loader_init_android
;; adds .next for khr_loader_init

;;; 12.16. XR_KHR_opengl_enable
;; todo: session creation for xlib,wayland,xcb
(defun get-opengl-graphics-requirements-khr (system-id)
  (cffi:with-foreign-object (p '(:struct %:graphics-requirements-opengl-khr))
    (cffi:with-foreign-slots ((%:type
                               %:next
                               %:min-api-version-supported
                               %:max-api-version-supported)
                              p (:struct %:graphics-requirements-opengl-khr))
      (setf %:type :type-graphics-requirements-opengl-khr
            %:next (cffi:null-pointer)
            %:min-api-version-supported 0
            %:max-api-version-supported 0)
      (check-result (%:get-opengl-graphics-requirements-khr
                     *instance* system-id p))
      (let ((min %:min-api-version-supported)
            (max %:max-api-version-supported))
        (list :min-api (list (%:version-major min)
                             (%:version-minor min)
                             (%:version-patch min))
              :max-api (list (%:version-major max)
                             (%:version-minor max)
                             (%:version-patch max)))))))


;;; 12.17. XR_KHR_opengl_es_enable
#+todo(defun get-opengl-es-graphics-requirements-khr)
;; also handle graphics requirements, creating sessions and enumerating images

;;; 12.18. XR_KHR_swapchain_usage_input_attachment_bit


;;; 12.19. XR_KHR_visibility_mask
#+todo (defun get-visibility-mask-khr ...)


;;; 12.20. XR_KHR_vulkan_enable
#+todo(defun %:get-vulkan-graphics-requirements-khr ...)
#+todo(defun %:get-vulkan-graphics-device-khr)
#+todo(defun %:get-vulkan-instance-extensions-khr ...)
#+todo(defun %:get-vulkan-device-extensions-khr ...)
;; also handle graphics requirements, creating sessions and enumerating images

;;; 12.21. XR_KHR_vulkan_enable2
#+todo(defun %:get-vulkan-graphics-requirements-2-khr ...)
#+todo(defun %:create-vulkan-instance-khr ...)
#+todo(defun %:get-vulkan-graphics-device-2-khr)
#+todo(defun %:create-vulkan-device-khr ...)
;; also handle graphics requirements, creating sessions and enumerating images


;;; 12.22. XR_KHR_vulkan_swapchain_format_list


;;; 12.23. XR_KHR_win32_convert_performance_counter_time

(defun convert-win32-performance-counter-to-time-khr (counter)
  (cffi:with-foreign-object (pc :int64)
    (setf (cffi:mem-ref pc :int64) counter)
    (with-returned-atom (pt %:time)
      (%:convert-win32-performance-counter-to-time-khr *instance* pc pt))))

(defun convert-time-to-win32-performance-counter-khr (time)
  (with-returned-atom (pc :int64)
    (%:convert-time-to-win32-performance-counter-khr *instance* time pc)))

;;; 12.24. XR_EXT_active_action_set_priority
;; adds .next to ACTIONS-SYNC-INFO for SYNC-ACTIONS

;;; 12.25. XR_EXT_conformance_automation

;;; 12.26. XR_EXT_debug_utils
(defun set-debug-utils-object-name-ext (handle type name)
  (with-debug-utils-object-name-info-ext (duonie
                                          :object-type type
                                          :object-handle handle
                                          :object-name name)
    (check-result (%:set-debug-utils-object-name-ext *instance* duonie))))


;; todo: decide on a user API for translating the C callback to lisp
;; callbacks
(defun create-debug-utils-messenger-ext (instance
                                         &key message-severities message-types
                                           user-callback (user-data 0))
  (with-debug-utils-messenger-create-info-ext
      (p :message-severities message-severities
         :message-types message-types
         :user-callback user-callback
         :user-data user-data)
    (cffi:with-foreign-object (messenger '%::debug-utils-messenger-ext)
      (let ((r (%::create-debug-utils-messenger-ext instance p messenger)))
        (unless (unqualified-success r)
          (xr-error r "create debug utils messenger failed ~s?"
                    (cffi:foreign-enum-keyword '%::%result r :errorp nil)))
        (when *create-verbose*
          (format *debug-io* "~&create debug utils messenger ~x~%" (cffi:mem-ref messenger '%::debug-utils-messenger-ext)))
        (cffi:mem-ref messenger '%::debug-utils-messenger-ext)))))


#++
(import-export %:destroy-debug-utils-messenger-ext)

(defun submit-debug-utils-message-ext (message
                                       &key
                                         ;; todo: objects, labels
                                         message-id function-name
                                         (severity :verbose-ext)
                                         (type :general-ext))
  (with-debug-utils-messenger-callback-data-ext
      (d :message-id (or message-id (cffi:null-pointer))
         :function-name (or function-name (cffi:null-pointer))
         :message message
         :object-count 0
         :objects (cffi:null-pointer)
         :session-label-count 0
         :session-labels (cffi:null-pointer))
    (check-result
     (%:submit-debug-utils-message-ext *instance* severity type d))))

(defun session-begin-debug-utils-label-region-ext (session label)
  (with-debug-utils-label-ext (d :label-name label)
    (%:session-begin-debug-utils-label-region-ext session d)))

(import-export %:session-end-debug-utils-label-region-ext)

(defmacro with-debug-utils-label ((session label) &body body)
  (a:once-only (session label)
    `(unwind-protect
          (progn
            (session-begin-debug-utils-label-region-ext ,session ,label)
            ,@body)
       (session-end-debug-utils-label-region-ext ,session))))

(defun session-insert-debug-utils-label-ext (session label)
  (with-debug-utils-label-ext (d :label-name label)
    (%:session-insert-debug-utils-label-ext session d)))


;;; 12.27. XR_EXT_dpad_binding


;;; 12.28. XR_EXT_eye_gaze_interaction


;;; 12.29. XR_EXT_hand_joints_motion_range


;;; 12.30. XR_EXT_hand_tracking

;; wrapper for hand-tracker handle, since we need to know how many
;; joints to allocate, which depends on the joint-set
(defstruct hand-tracker
  (handle nil :type (or null (unsigned-byte 64)))
  (joint-count %:+hand-joint-count-ext+ :type (unsigned-byte 32)))

(defun create-hand-tracker-ext (session hand &key (joint-set :default-ext))
  (with-hand-tracker-create-info-ext (ci :hand hand
                                         :hand-joint-set joint-set)
    ;; calculate joint count first so we error before creating handle
    (let ((c (ecase joint-set
               (:default-ext
                %:+hand-joint-count-ext+)
               (:hand-with-forearm-ultraleap
                %:+hand-forearm-joint-count-ultraleap+))))
      (make-hand-tracker
       :handle (with-returned-handle (h %:hand-tracker-ext)
                 (%:create-hand-tracker-ext session ci h))
       :joint-count c))))

(defun destroy-hand-tracker-ext (hand-tracker)
  (%:destroy-hand-tracker-ext (hand-tracker-handle hand-tracker)))

(defun locate-hand-joints-ext (hand-tracker base-space time)
  (let ((h (hand-tracker-handle hand-tracker))
        (n (hand-tracker-joint-count hand-tracker)))
    ;; todo: optionally? add velocities
    (with-hand-joints-locate-info-ext (li :base-space base-space :time time)
      (cffi:with-foreign-object (loc-array '(:struct %:hand-joint-location-ext)
                                 n)
        (with-hand-joint-locations-ext (l :%slots t
                                          :is-active 0
                                          :joint-count n
                                          :joint-locations loc-array)
          (check-result (%:locate-hand-joints-ext h li l))
          ;; todo: optionally pass an array of structs to fill instead
          ;; of consing new one every time?
          (let ((r (make-array n :initial-element nil)))
            (when %:is-active
              (loop for i below n
                    for p = (cffi:mem-aptr
                             loc-array '(:struct %:hand-joint-location-ext) i)
                    do (cffi:with-foreign-slots (((pose :pointer %:pose)
                                                  %:radius
                                                  %:location-flags)
                                                 p (:struct
                                                    %:hand-joint-location-ext))
                         (setf (aref r i)
                               ;; fixme: decide on better API for this
                               ;; (maybe unconditionally query
                               ;; velocities, and make a
                               ;; pose+vel+radius for this?
                               (list :pose (make-pose-from-pointer
                                            pose %:location-flags)
                                     :radius %:radius)))))
            r))))))


;;; 12.51. XR_FB_display_refresh_rate

(defun enumerate-display-refresh-rates-fb (session)
  (with-two-call (i o p :float)
    (%:enumerate-display-refresh-rates-fb session i o p)))

(defun get-display-refresh-rate-fb (session)
  (with-returned-atom (p :float)
    (%:get-display-refresh-rate-fb session p)))


(defun request-display-refresh-rate-fb (session rate)
  (%:request-display-refresh-rate-fb session rate))

(defun (setf get-display-refresh-rate-fb) (new-rate session)
  (%:request-display-refresh-rate-fb session new-rate)
  new-rate)
