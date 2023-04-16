(in-package #:3b-openxr-bindings)
;; Copyright (c) 2017-2023, The Khronos Group Inc.
;;
;; SPDX-License-Identifier: Apache-2.0 OR MIT
;;
;; ------------------------------------------------------------------------
;;
;; This file, xr.xml, is the OpenXR API Registry. It is a critically important
;; and normative part of the OpenXR Specification, including a canonical
;; machine-readable definition of the API, parameter and member validation
;; language incorporated into the Specification and reference pages, and other
;; material which is registered by Khronos, such as tags used by extension and
;; layer authors. The only authoritative version of xr.xml is the one
;; maintained in the default branch of the Khronos OpenXR GitHub project.
;; globals without an enum
;; Value types
(defctype bool-32 :uint32)

(defctype flags-64 :uint64)

(defctype time :int64)

(defctype duration :int64)

(defctype version :uint64)

;; XR_FB_spatial_entity_user
(defctype space-user-id-fb :uint64)

;; Atoms, allocated by the runtime (if implementation requires) and never freed
(defctype path atom)

(defctype system-id atom)

;; XR_MSFT_controller_model
(defctype controller-model-key-msft atom)

;; XR_FB_spatial_entity
(defctype async-request-id-fb atom)

;; XR_FB_render_model
(defctype render-model-key-fb atom)

;; Bitmask types
;; Flags
(defbitfield* (instance-create-flags flags-64))

(defbitfield* (session-create-flags flags-64))

(defbitfield* (swapchain-create-flags flags-64)
  ;; Content will be protected from CPU access
  (:protected-content #x00000001)
  ;; Only one image will be acquired from this swapchain over its lifetime
  (:static-image #x00000002))

(defbitfield* (swapchain-usage-flags flags-64)
  ;; Specifies that the image may: be a color rendering target.
  (:color-attachment #x00000001)
  ;; Specifies that the image may: be a depth/stencil rendering target.
  (:depth-stencil-attachment #x00000002)
  ;; Specifies that the image may: be accessed out of order and that access may: be via atomic operations.
  (:unordered-access #x00000004)
  ;; Specifies that the image may: be used as the source of a transfer operation.
  (:transfer-src #x00000008)
  ;; Specifies that the image may: be used as the destination of a transfer operation.
  (:transfer-dst #x00000010)
  ;; Specifies that the image may: be sampled by a shader.
  (:sampled #x00000020)
  ;; Specifies that the image may: be reinterpreted as another image format.
  (:mutable-format #x00000040)
;; NOTE: This was added before the working group established a process, and has been normalized by promotion to KHR. Future vendor extensions MUST NOT extend core bitmasks.
  ;; Specifies that the image may: be used as a input attachment.
  (:input-attachment-mnd #x00000080)
  ;; Specifies that the image may: be used as a input attachment.
  (:input-attachment-khr #x00000080))

(defbitfield* (view-state-flags flags-64)
  ;; Indicates validity of all slink:XrView orientations
  (:orientation-valid #x00000001)
  ;; Indicates validity of all slink:XrView positions
  (:position-valid #x00000002)
  ;; Indicates whether all slink:XrView orientations are actively tracked
  (:orientation-tracked #x00000004)
  ;; Indicates whether all slink:XrView positions are actively tracked
  (:position-tracked #x00000008))

(defbitfield* (composition-layer-flags flags-64)
  ;; Enables chromatic aberration correction when not done by default. This flag has no effect on any known conformant runtime, and is planned for deprecation for OpenXR 1.1
  (:correct-chromatic-aberration #x00000001)
  ;; Enables the layer texture alpha channel.
  (:blend-texture-source-alpha #x00000002)
  ;; Indicates the texture color channels have not been premultiplied by the texture alpha channel.
  (:unpremultiplied-alpha #x00000004))

(defbitfield* (space-location-flags flags-64)
  ;; Indicates that the pname:orientation member contains valid data
  (:orientation-valid #x00000001)
  ;; Indicates that the pname:position member contains valid data
  (:position-valid #x00000002)
  ;; Indicates whether pname:pose member contains an actively tracked pname:orientation
  (:orientation-tracked #x00000004)
  ;; Indicates whether pname:pose member contains an actively tracked pname:position
  (:position-tracked #x00000008))

(defbitfield* (space-velocity-flags flags-64)
  ;; Indicates that the pname:linearVelocity member contains valid data. Applications must: not read the pname:linearVelocity field if this flag is unset.
  (:linear-valid #x00000001)
  ;; Indicates that the pname:angularVelocity member contains valid data. Applications must: not read the pname:angularVelocity field if this flag is unset.
  (:angular-valid #x00000002))

(defbitfield* (input-source-localized-name-flags flags-64)
  ;; Asks for the part of the string which indicates the top level user path the source represents
  (:user-path #x00000001)
  ;; Asks for the part of the string which represents the interaction profile of the source
  (:interaction-profile #x00000002)
  ;; Asks for the part of the string which represents the component on the device which needs to be interacted with
  (:component #x00000004))

;; Bitmask types for XR_KHR_vulkan_enable2
;; flags for XR_EXT_debug_utils
(defbitfield* (vulkan-instance-create-flags-khr flags-64))

(defbitfield* (vulkan-device-create-flags-khr flags-64))

;; Bitmask types for XR_EXT_debug_utils
;; flags for XR_EXT_debug_utils
(defbitfield* (debug-utils-message-severity-flags-ext flags-64)
  ;; Most verbose output severity, typically used for debugging.
  (:verbose-ext #x00000001)
  ;; General info message
  (:info-ext #x00000010)
  ;; Indicates the item may be the cause of issues.
  (:warning-ext #x00000100)
  ;; Indicates that the item is definitely related to erroneous behavior.
  (:error-ext #x00001000))

(defbitfield* (debug-utils-message-type-flags-ext flags-64)
  ;; Indicates this is a general message
  (:general-ext #x00000001)
  ;; Indicates the message is related to a validation message
  (:validation-ext #x00000002)
  ;; Indicates the message is related to a potential performance situation
  (:performance-ext #x00000004)
  ;; Indicates the message is related to a non-conformant runtime result
  (:conformance-ext #x00000008))

;; Bitmask types for XR_EXTX_overlay
;; flags for XR_EXTX_overlay
(defbitfield* (overlay-main-session-flags-extx flags-64)
  ;; Indicates the main session enabled `XR_KHR_composition_layer_depth`
  (:enabled-composition-layer-info-depth-extx #x00000001))

(defbitfield* (overlay-session-create-flags-extx flags-64))

;; Bitmask types for XR_FB_android_surface_swapchain_create
;; flags for XR_FB_android_surface_swapchain_create
(defbitfield* (android-surface-swapchain-flags-fb flags-64)
  ;; Create the underlying BufferQueue in synchronous mode
  (:synchronous-fb #x00000001)
  ;; Acquire most recent buffer whose presentation timestamp is not greater than display time of final composited frame
  (:use-timestamps-fb #x00000002))

;; Bitmask types for XR_FB_composition_layer_image_layout
;; flags for XR_FB_composition_layer_image_layout
(defbitfield* (composition-layer-image-layout-flags-fb flags-64)
  ;; The coordinate origin of the swapchain image must be considered to be flipped vertically.
  (:vertical-flip-fb #x00000001))

;; Bitmask types for XR_FB_composition_layer_secure_content
;; flags for XR_FB_composition_layer_secure_content
(defbitfield* (composition-layer-secure-content-flags-fb flags-64)
  ;; Indicates the layer will only be visible inside the HMD, and not visible to external sources
  (:exclude-layer-fb #x00000001)
  ;; Indicates the layer will be displayed inside the HMD, but replaced by proxy content when written to external sources
  (:replace-layer-fb #x00000002))

;; Bitmask types for XR_FB_foveation
;; flags for XR_FB_foveation
(defbitfield* (swapchain-create-foveation-flags-fb flags-64)
  ;; Explicitly create the swapchain with scaled bin foveation support. The application must ensure that the swapchain is using the OpenGL graphics API and that the QCOM_texture_foveated extension is supported and enabled.
  (:scaled-bin-fb #x00000001)
  ;; Explicitly create the swapchain with fragment density map foveation support. The application must ensure that the swapchain is using the Vulkan graphics API and that the VK_EXT_fragment_density_map extension is supported and enabled.
  (:ragment-density-map-fb #x00000002))

(defbitfield* (swapchain-state-foveation-flags-fb flags-64))

;; Bitmask types for XR_META_foveation_eye_tracked
;; flags for XR_META_foveation_eye_tracked
(defbitfield* (foveation-eye-tracked-profile-create-flags-meta flags-64))

(defbitfield* (foveation-eye-tracked-state-flags-meta flags-64)
  ;; Indicates whether or not foveation data is valid. This can happen if the eye tracker is obscured, the camera has dirt, or eye lid is closed, etc.
  (:valid-meta #x00000001))

;; Bitmask types for XR_FB_triangle_mesh
(defbitfield* (triangle-mesh-flags-fb flags-64)
  ;; The triangle mesh is mutable (can be modified after it is created).
  (:mutable-fb #x00000001))

;; Bitmask types for XR_FB_passthrough
(defbitfield* (passthrough-flags-fb flags-64)
  ;; The object (passthrough, layer) is running at creation.
  (:is-running-at-creation-fb #x00000001)
  ;; The passthrough system sends depth information to the compositor. Only applicable to layer objects.
  (:layer-depth-fb #x00000002))

(defbitfield* (passthrough-state-changed-flags-fb flags-64)
  ;; Passthrough system requires reinitialization.
  (:reinit-required-fb #x00000001)
  ;; Non-recoverable error has occurred. A device reboot or a firmware update may be required.
  (:non-recoverable-error-fb #x00000002)
  ;; A recoverable error has occurred. The runtime will attempt to recover, but some functionality may be temporarily unavailable.
  (:recoverable-error-fb #x00000004)
  ;; The runtime has recovered from a previous error and is functioning normally.
  (:restored-error-fb #x00000008))

(defbitfield* (passthrough-capability-flags-fb flags-64)
  ;; The system supports passthrough.
  (:bit-fb #x00000001)
  ;; The system can show passthrough with realistic colors. ename:XR_PASSTHROUGH_CAPABILITY_BIT_FB must: be set if ename:XR_PASSTHROUGH_CAPABILITY_COLOR_BIT_FB is set.
  (:color-fb #x00000002)
  ;; The system supports passthrough layers composited using depth testing. ename:XR_PASSTHROUGH_CAPABILITY_BIT_FB must: be set if ename:XR_PASSTHROUGH_CAPABILITY_LAYER_DEPTH_BIT_FB is set.
  (:layer-depth-fb #x00000004))

;; Bitmask types for XR_FB_hand_tracking_aim
;; flags for XR_FB_hand_tracking_aim
(defbitfield* (hand-tracking-aim-flags-fb flags-64)
  ;; Aiming data is computed from additional sources beyond the hand data in the base structure
  (:computed-fb #x00000001)
  ;; Aiming data is valid
  (:valid-fb #x00000002)
  ;; Index finger pinch discrete signal
  (:index-pinching-fb #x00000004)
  ;; Middle finger pinch discrete signal
  (:middle-pinching-fb #x00000008)
  ;; Ring finger pinch discrete signal
  (:ring-pinching-fb #x00000010)
  ;; Little finger pinch discrete signal
  (:little-pinching-fb #x00000020)
  ;; System gesture is active
  (:system-gesture-fb #x00000040)
  ;; Hand is currently marked as dominant for the system
  (:dominant-hand-fb #x00000080)
  ;; System menu gesture is active
  (:menu-pressed-fb #x00000100))

;; Bitmask types for XR_FB_keyboard_tracking
;; flags for XR_FB_keyboard_tracking query state
(defbitfield* (keyboard-tracking-flags-fb flags-64)
  ;; indicates that the system has a physically tracked keyboard to report.  If not set then no other bits should be considered to be valid or meaningful.  If set either XR_KEYBOARD_TRACKING_LOCAL_BIT_FB or XR_KEYBOARD_TRACKING_REMOTE_BIT_FB must also be set.
  (:exists-fb #x00000001)
  ;; indicates that the physically tracked keyboard is intended to be used in a local pairing with the system.  Mutally exclusive with XR_KEYBOARD_TRACKING_REMOTE_BIT_FB.
  (:local-fb #x00000002)
  ;; indicates that the physically tracked keyboard is intended to be used while paired to a separate remote computing device. Mutally exclusive with XR_KEYBOARD_TRACKING_LOCAL_BIT_FB.
  (:remote-fb #x00000004)
  ;; indicates that the physically tracked keyboard is actively connected to the headset and capable of sending key data
  (:connected-fb #x00000008))

;; flags for XR_FB_keyboard_tracking query
(defbitfield* (keyboard-tracking-query-flags-fb flags-64)
  ;; indicates the query is for the physically tracked keyboard that is intended to be used in a local pairing with the System. Mutally exclusive with XR_KEYBOARD_TRACKING_QUERY_REMOTE_BIT_FB.
  (:local-fb #x00000002)
  ;; indicates the query is for the physically tracked keyboard that may be connected to a separate remote computing device. Mutally exclusive with XR_KEYBOARD_TRACKING_QUERY_LOCAL_BIT_FB.
  (:remote-fb #x00000004))

;; Bitmask types for XR_FB_space_warp
;; flags for XR_FB_space_warp
(defbitfield* (composition-layer-space-warp-info-flags-fb flags-64)
  ;; Skip current frame's space warp extrapolation
  (:rame-skip-fb #x00000001))

;; Bitmask types for XR_FB_render_model
;; flags for XR_FB_render_model
(defbitfield* (render-model-flags-fb flags-64)
  ;; Minimal level of support.  Can only contain a single mesh.  Can only contain a single texture.  Can not contain transparency.  Assumes unlit rendering.  Requires Extension KHR_texturebasisu.
  (:supports-gltf-2-0-subset-1-fb #x00000001)
  ;; All of XR_RENDER_MODEL_SUPPORTS_GLTF_2_0_SUBSET_1_BIT_FB support plus: Multiple meshes. Multiple Textures. Texture Transparency.
  (:supports-gltf-2-0-subset-2-fb #x00000002))

;; Bitmask types for XR_ALMALENCE_digital_lens_control
;; flags for XR_ALMALENCE_digital_lens_control
(defbitfield* (digital-lens-control-flags-almalence flags-64)
  ;; disables Digital Lens processing of render textures
  (:processing-disable-almalence #x00000001))

;; Bitmask types for XR_FB_composition_layer_settings
;; flags for XR_FB_composition_layer_settings
(defbitfield* (composition-layer-settings-flags-fb flags-64)
  ;; Indicates compositor may: use layer texture supersampling.
  (:normal-super-sampling-fb #x00000001)
  ;; Indicates compositor may: use high quality layer texture supersampling.
  (:quality-super-sampling-fb #x00000002)
  ;; Indicates compositor may: use layer texture sharpening.
  (:normal-sharpening-fb #x00000004)
  ;; Indicates compositor may: use high quality layer texture sharpening.
  (:quality-sharpening-fb #x00000008))

;; Bitmask types for XR_OCULUS_external_camera
;; flags for XR_OCULUS_external_camera
(defbitfield* (external-camera-status-flags-oculus flags-64)
  ;; External camera is connected
  (:connected-oculus #x00000001)
  ;; External camera is undergoing calibration
  (:calibrating-oculus #x00000002)
  ;; External camera has tried and failed calibration
  (:calibration-failed-oculus #x00000004)
  ;; External camera has tried and passed calibration
  (:calibrated-oculus #x00000008)
  ;; External camera is capturing
  (:capturing-oculus #x00000010))

;; Bitmask types for XR_META_performance_metrics
;; flags for XR_META_performance_metrics
(defbitfield* (performance-metrics-counter-flags-meta flags-64)
  ;; Indicates any of the values in XrPerformanceMetricsCounterMETA is valid.
  (:any-value-valid-meta #x00000001)
  ;; Indicates the uintValue in XrPerformanceMetricsCounterMETA is valid.
  (:uint-value-valid-meta #x00000002)
  ;; Indicates the floatValue in XrPerformanceMetricsCounterMETA is valid.
  (:oat-value-valid-meta #x00000004))

;; Bitmask types for XR_HTC_foveation
(defbitfield* (foveation-dynamic-flags-htc flags-64)
  ;; Allow system to set periphery pixel density dynamically.
  (:level-enabled-htc #x00000001)
  ;; Allow system to set clear FOV degree dynamically.
  (:clear-fov-enabled-htc #x00000002)
  ;; Allow system to set focal center offset dynamically.
  (:ocal-center-offset-enabled-htc #x00000004))

;; Bitmask types for XR_ML_frame_end_info
;; enums for XR_ML_frame_end_info
(defbitfield* (frame-end-info-flags-ml flags-64)
  ;; Indicates that the content for this frame is protected and should not be recorded or captured outside the graphics system.
  (:protected-ml #x00000001)
  ;; Indicates that a soft fade to transparent should be added to the frame in the compositor to blend any hard edges at the FOV limits.
  (:vignette-ml #x00000002))

;; Bitmask types for XR_ML_global_dimmer
;; enums for XR_ML_global_dimmer
(defbitfield* (global-dimmer-frame-end-info-flags-ml flags-64)
  ;; Indicates that the global dimmer should: be enabled and controlled by slink:XrGlobalDimmerFrameEndInfoML::pname:dimmerValue.
  (:enabled-ml #x00000001))

;; Handles referring to internally-maintained objects.
;; These types which can be 64-bit integers or opaque pointers, selected at compile time based on pointer size
(defctype instance xr-handle)

(defctype session xr-handle)

(defctype action-set xr-handle)

(defctype action xr-handle)

(defctype swapchain xr-handle)

(defctype space xr-handle)

(defctype debug-utils-messenger-ext xr-handle)

(defctype spatial-anchor-msft xr-handle)

;; XR_EXT_hand_tracking
(defctype hand-tracker-ext xr-handle)

;; XR_FB_foveation
(defctype foveation-profile-fb xr-handle)

;; XR_FB_triangle_mesh
(defctype triangle-mesh-fb xr-handle)

;; XR_FB_passthrough
(defctype passthrough-fb xr-handle)

(defctype passthrough-layer-fb xr-handle)

(defctype geometry-instance-fb xr-handle)

;; XR_HTC_facial_tracking
(defctype facial-tracker-htc xr-handle)

;; XR_HTC_passthrough
(defctype passthrough-htc xr-handle)

;; XR_FB_face_tracking
(defctype face-tracker-fb xr-handle)

;; XR_FB_body_tracking
(defctype body-tracker-fb xr-handle)

;; XR_FB_eye_tracking_social
(defctype eye-tracker-fb xr-handle)

;; XR_FB_spatial_entity_user
(defctype space-user-fb xr-handle)

;; enums and flag bits generated from corresponding <enums> tags below
;; Unlike OpenGL, most tokens in OpenXR are actual typed enumerants in
;;          their own numeric namespaces. The "name" attribute is the C enum
;;          type name, and is pulled in from a <type> definition above
;;          (slightly clunky, but retains the type / enum distinction). "type"
;;          attributes of "enum" or "bitmask" indicate that these values should
;;          be generated inside an appropriate definition.
;; Structure type enumerant
(defcenum structure-type
  (:type-unknown 0)
  (:type-api-layer-properties 1)
  (:type-extension-properties 2)
  (:type-instance-create-info 3)
  (:type-system-get-info 4)
  (:type-system-properties 5)
  (:type-view-locate-info 6)
  (:type-view 7)
  (:type-session-create-info 8)
  (:type-swapchain-create-info 9)
  (:type-session-begin-info 10)
  (:type-view-state 11)
  (:type-frame-end-info 12)
  (:type-haptic-vibration 13)
  (:type-event-data-buffer 16)
  (:type-event-data-instance-loss-pending 17)
  (:type-event-data-session-state-changed 18)
  (:type-action-state-boolean 23)
  (:type-action-state-float 24)
  (:type-action-state-vector2f 25)
  (:type-action-state-pose 27)
  (:type-action-set-create-info 28)
  (:type-action-create-info 29)
  (:type-instance-properties 32)
  (:type-frame-wait-info 33)
  (:type-composition-layer-projection 35)
  (:type-composition-layer-quad 36)
  (:type-reference-space-create-info 37)
  (:type-action-space-create-info 38)
  (:type-event-data-reference-space-change-pending 40)
  (:type-view-configuration-view 41)
  (:type-space-location 42)
  (:type-space-velocity 43)
  (:type-frame-state 44)
  (:type-view-configuration-properties 45)
  (:type-frame-begin-info 46)
  (:type-composition-layer-projection-view 48)
  (:type-event-data-events-lost 49)
  (:type-interaction-profile-suggested-binding 51)
  (:type-event-data-interaction-profile-changed 52)
  (:type-interaction-profile-state 53)
  (:type-swapchain-image-acquire-info 55)
  (:type-swapchain-image-wait-info 56)
  (:type-swapchain-image-release-info 57)
  (:type-action-state-get-info 58)
  (:type-haptic-action-info 59)
  (:type-session-action-sets-attach-info 60)
  (:type-actions-sync-info 61)
  (:type-bound-sources-for-action-enumerate-info 62)
  (:type-input-source-localized-name-get-info 63)
  (:type-composition-layer-cube-khr 1000006000)
  (:type-instance-create-info-android-khr 1000008000)
  (:type-composition-layer-depth-info-khr 1000010000)
  (:type-vulkan-swapchain-format-list-create-info-khr 1000014000)
  (:type-event-data-perf-settings-ext 1000015000)
  (:type-composition-layer-cylinder-khr 1000017000)
  (:type-composition-layer-equirect-khr 1000018000)
  (:type-debug-utils-object-name-info-ext 1000019000)
  (:type-debug-utils-messenger-callback-data-ext 1000019001)
  (:type-debug-utils-messenger-create-info-ext 1000019002)
  (:type-debug-utils-label-ext 1000019003)
  (:type-graphics-binding-opengl-win32-khr 1000023000)
  (:type-graphics-binding-opengl-xlib-khr 1000023001)
  (:type-graphics-binding-opengl-xcb-khr 1000023002)
  (:type-graphics-binding-opengl-wayland-khr 1000023003)
  (:type-swapchain-image-opengl-khr 1000023004)
  (:type-graphics-requirements-opengl-khr 1000023005)
  (:type-graphics-binding-opengl-es-android-khr 1000024001)
  (:type-swapchain-image-opengl-es-khr 1000024002)
  (:type-graphics-requirements-opengl-es-khr 1000024003)
  (:type-graphics-binding-vulkan-khr 1000025000)
  (:type-swapchain-image-vulkan-khr 1000025001)
  (:type-graphics-requirements-vulkan-khr 1000025002)
  (:type-graphics-binding-d3d11-khr 1000027000)
  (:type-swapchain-image-d3d11-khr 1000027001)
  (:type-graphics-requirements-d3d11-khr 1000027002)
  (:type-graphics-binding-d3d12-khr 1000028000)
  (:type-swapchain-image-d3d12-khr 1000028001)
  (:type-graphics-requirements-d3d12-khr 1000028002)
  (:type-system-eye-gaze-interaction-properties-ext 1000030000)
  (:type-eye-gaze-sample-time-ext 1000030001)
  (:type-visibility-mask-khr 1000031000)
  (:type-event-data-visibility-mask-changed-khr 1000031001)
  (:type-session-create-info-overlay-extx 1000033000)
  (:type-event-data-main-session-visibility-changed-extx 1000033003)
  (:type-composition-layer-color-scale-bias-khr 1000034000)
  (:type-spatial-anchor-create-info-msft 1000039000)
  (:type-spatial-anchor-space-create-info-msft 1000039001)
  (:type-composition-layer-image-layout-fb 1000040000)
  (:type-composition-layer-alpha-blend-fb 1000041001)
  (:type-view-configuration-depth-range-ext 1000046000)
  (:type-graphics-binding-egl-mndx 1000048004)
  (:type-spatial-graph-node-space-create-info-msft 1000049000)
  (:type-spatial-graph-static-node-binding-create-info-msft 1000049001)
  (:type-spatial-graph-node-binding-properties-get-info-msft 1000049002)
  (:type-spatial-graph-node-binding-properties-msft 1000049003)
  (:type-system-hand-tracking-properties-ext 1000051000)
  (:type-hand-tracker-create-info-ext 1000051001)
  (:type-hand-joints-locate-info-ext 1000051002)
  (:type-hand-joint-locations-ext 1000051003)
  (:type-hand-joint-velocities-ext 1000051004)
  (:type-system-hand-tracking-mesh-properties-msft 1000052000)
  (:type-hand-mesh-space-create-info-msft 1000052001)
  (:type-hand-mesh-update-info-msft 1000052002)
  (:type-hand-mesh-msft 1000052003)
  (:type-hand-pose-type-info-msft 1000052004)
  (:type-secondary-view-configuration-session-begin-info-msft 1000053000)
  (:type-secondary-view-configuration-state-msft 1000053001)
  (:type-secondary-view-configuration-frame-state-msft 1000053002)
  (:type-secondary-view-configuration-frame-end-info-msft 1000053003)
  (:type-secondary-view-configuration-layer-info-msft 1000053004)
  (:type-secondary-view-configuration-swapchain-create-info-msft 1000053005)
  (:type-controller-model-key-state-msft 1000055000)
  (:type-controller-model-node-properties-msft 1000055001)
  (:type-controller-model-properties-msft 1000055002)
  (:type-controller-model-node-state-msft 1000055003)
  (:type-controller-model-state-msft 1000055004)
  (:type-view-configuration-view-fov-epic 1000059000)
  (:type-holographic-window-attachment-msft 1000063000)
  (:type-composition-layer-reprojection-info-msft 1000066000)
  (:type-composition-layer-reprojection-plane-override-msft 1000066001)
  (:type-android-surface-swapchain-create-info-fb 1000070000)
  (:type-composition-layer-secure-content-fb 1000072000)
  (:type-body-tracker-create-info-fb 1000076001)
  (:type-body-joints-locate-info-fb 1000076002)
  (:type-system-body-tracking-properties-fb 1000076004)
  (:type-body-joint-locations-fb 1000076005)
  (:type-body-skeleton-fb 1000076006)
  (:type-interaction-profile-dpad-binding-ext 1000078000)
  (:type-interaction-profile-analog-threshold-valve 1000079000)
  (:type-hand-joints-motion-range-info-ext 1000080000)
  (:type-loader-init-info-android-khr 1000089000)
  (:type-vulkan-instance-create-info-khr 1000090000)
  (:type-vulkan-device-create-info-khr 1000090001)
  (:type-vulkan-graphics-device-get-info-khr 1000090003)
  (:type-graphics-binding-vulkan2-khr 1000025000)
  (:type-swapchain-image-vulkan2-khr 1000025001)
  (:type-graphics-requirements-vulkan2-khr 1000025002)
  (:type-composition-layer-equirect2-khr 1000091000)
  (:type-scene-observer-create-info-msft 1000097000)
  (:type-scene-create-info-msft 1000097001)
  (:type-new-scene-compute-info-msft 1000097002)
  (:type-visual-mesh-compute-lod-info-msft 1000097003)
  (:type-scene-components-msft 1000097004)
  (:type-scene-components-get-info-msft 1000097005)
  (:type-scene-component-locations-msft 1000097006)
  (:type-scene-components-locate-info-msft 1000097007)
  (:type-scene-objects-msft 1000097008)
  (:type-scene-component-parent-filter-info-msft 1000097009)
  (:type-scene-object-types-filter-info-msft 1000097010)
  (:type-scene-planes-msft 1000097011)
  (:type-scene-plane-alignment-filter-info-msft 1000097012)
  (:type-scene-meshes-msft 1000097013)
  (:type-scene-mesh-buffers-get-info-msft 1000097014)
  (:type-scene-mesh-buffers-msft 1000097015)
  (:type-scene-mesh-vertex-buffer-msft 1000097016)
  (:type-scene-mesh-indices-uint32-msft 1000097017)
  (:type-scene-mesh-indices-uint16-msft 1000097018)
  (:type-serialized-scene-fragment-data-get-info-msft 1000098000)
  (:type-scene-deserialize-info-msft 1000098001)
  (:type-event-data-display-refresh-rate-changed-fb 1000101000)
  (:type-vive-tracker-paths-htcx 1000103000)
  (:type-event-data-vive-tracker-connected-htcx 1000103001)
  (:type-system-facial-tracking-properties-htc 1000104000)
  (:type-facial-tracker-create-info-htc 1000104001)
  (:type-facial-expressions-htc 1000104002)
  (:type-system-color-space-properties-fb 1000108000)
  (:type-hand-tracking-mesh-fb 1000110001)
  (:type-hand-tracking-scale-fb 1000110003)
  (:type-hand-tracking-aim-state-fb 1000111001)
  (:type-hand-tracking-capsules-state-fb 1000112000)
  (:type-system-spatial-entity-properties-fb 1000113004)
  (:type-spatial-anchor-create-info-fb 1000113003)
  (:type-space-component-status-set-info-fb 1000113007)
  (:type-space-component-status-fb 1000113001)
  (:type-event-data-spatial-anchor-create-complete-fb 1000113005)
  (:type-event-data-space-set-status-complete-fb 1000113006)
  (:type-foveation-profile-create-info-fb 1000114000)
  (:type-swapchain-create-info-foveation-fb 1000114001)
  (:type-swapchain-state-foveation-fb 1000114002)
  (:type-foveation-level-profile-create-info-fb 1000115000)
  (:type-keyboard-space-create-info-fb 1000116009)
  (:type-keyboard-tracking-query-fb 1000116004)
  (:type-system-keyboard-tracking-properties-fb 1000116002)
  (:type-triangle-mesh-create-info-fb 1000117001)
  (:type-system-passthrough-properties-fb 1000118000)
  (:type-passthrough-create-info-fb 1000118001)
  (:type-passthrough-layer-create-info-fb 1000118002)
  (:type-composition-layer-passthrough-fb 1000118003)
  (:type-geometry-instance-create-info-fb 1000118004)
  (:type-geometry-instance-transform-fb 1000118005)
  (:type-system-passthrough-properties2-fb 1000118006)
  (:type-passthrough-style-fb 1000118020)
  (:type-passthrough-color-map-mono-to-rgba-fb 1000118021)
  (:type-passthrough-color-map-mono-to-mono-fb 1000118022)
  (:type-passthrough-brightness-contrast-saturation-fb 1000118023)
  (:type-event-data-passthrough-state-changed-fb 1000118030)
  (:type-render-model-path-info-fb 1000119000)
  (:type-render-model-properties-fb 1000119001)
  (:type-render-model-buffer-fb 1000119002)
  (:type-render-model-load-info-fb 1000119003)
  (:type-system-render-model-properties-fb 1000119004)
  (:type-render-model-capabilities-request-fb 1000119005)
  (:type-binding-modifications-khr 1000120000)
  (:type-view-locate-foveated-rendering-varjo 1000121000)
  (:type-foveated-view-configuration-view-varjo 1000121001)
  (:type-system-foveated-rendering-properties-varjo 1000121002)
  (:type-composition-layer-depth-test-varjo 1000122000)
  (:type-system-marker-tracking-properties-varjo 1000124000)
  (:type-event-data-marker-tracking-update-varjo 1000124001)
  (:type-marker-space-create-info-varjo 1000124002)
  (:type-frame-end-info-ml 1000135000)
  (:type-global-dimmer-frame-end-info-ml 1000136000)
  (:type-coordinate-space-create-info-ml 1000137000)
  (:type-spatial-anchor-persistence-info-msft 1000142000)
  (:type-spatial-anchor-from-persisted-anchor-create-info-msft 1000142001)
  (:type-space-query-info-fb 1000156001)
  (:type-space-query-results-fb 1000156002)
  (:type-space-storage-location-filter-info-fb 1000156003)
  (:type-space-uuid-filter-info-fb 1000156054)
  (:type-space-component-filter-info-fb 1000156052)
  (:type-event-data-space-query-results-available-fb 1000156103)
  (:type-event-data-space-query-complete-fb 1000156104)
  (:type-space-save-info-fb 1000158000)
  (:type-space-erase-info-fb 1000158001)
  (:type-event-data-space-save-complete-fb 1000158106)
  (:type-event-data-space-erase-complete-fb 1000158107)
  (:type-swapchain-image-foveation-vulkan-fb 1000160000)
  (:type-swapchain-state-android-surface-dimensions-fb 1000161000)
  (:type-swapchain-state-sampler-opengl-es-fb 1000162000)
  (:type-swapchain-state-sampler-vulkan-fb 1000163000)
  (:type-space-share-info-fb 1000169001)
  (:type-event-data-space-share-complete-fb 1000169002)
  (:type-composition-layer-space-warp-info-fb 1000171000)
  (:type-system-space-warp-properties-fb 1000171001)
  (:type-haptic-amplitude-envelope-vibration-fb 1000173001)
  (:type-semantic-labels-fb 1000175000)
  (:type-room-layout-fb 1000175001)
  (:type-boundary-2d-fb 1000175002)
  (:type-digital-lens-control-almalence 1000196000)
  (:type-event-data-scene-capture-complete-fb 1000198001)
  (:type-scene-capture-request-info-fb 1000198050)
  (:type-space-container-fb 1000199000)
  (:type-foveation-eye-tracked-profile-create-info-meta 1000200000)
  (:type-foveation-eye-tracked-state-meta 1000200001)
  (:type-system-foveation-eye-tracked-properties-meta 1000200002)
  (:type-system-face-tracking-properties-fb 1000201004)
  (:type-face-tracker-create-info-fb 1000201005)
  (:type-face-expression-info-fb 1000201002)
  (:type-face-expression-weights-fb 1000201006)
  (:type-eye-tracker-create-info-fb 1000202001)
  (:type-eye-gazes-info-fb 1000202002)
  (:type-eye-gazes-fb 1000202003)
  (:type-system-eye-tracking-properties-fb 1000202004)
  (:type-passthrough-keyboard-hands-intensity-fb 1000203002)
  (:type-composition-layer-settings-fb 1000204000)
  (:type-haptic-pcm-vibration-fb 1000209001)
  (:type-device-pcm-sample-rate-state-fb 1000209002)
  (:type-device-pcm-sample-rate-get-info-fb 1000209002)
  (:type-composition-layer-depth-test-fb 1000212000)
  (:type-local-dimming-frame-end-info-meta 1000216000)
  (:type-external-camera-oculus 1000226000)
  (:type-vulkan-swapchain-create-info-meta 1000227000)
  (:type-performance-metrics-state-meta 1000232001)
  (:type-performance-metrics-counter-meta 1000232002)
  (:type-space-list-save-info-fb 1000238000)
  (:type-event-data-space-list-save-complete-fb 1000238001)
  (:type-space-user-create-info-fb 1000241001)
  (:type-system-headset-id-properties-meta 1000245000)
  (:type-passthrough-create-info-htc 1000317001)
  (:type-passthrough-color-htc 1000317002)
  (:type-passthrough-mesh-transform-info-htc 1000317003)
  (:type-composition-layer-passthrough-htc 1000317004)
  (:type-foveation-apply-info-htc 1000318000)
  (:type-foveation-dynamic-mode-info-htc 1000318001)
  (:type-foveation-custom-mode-info-htc 1000318002)
  (:type-active-action-set-priorities-ext 1000373000)
  (:type-system-force-feedback-curl-properties-mndx 1000375000)
  (:type-force-feedback-curl-apply-locations-mndx 1000375001))

;; Error and return codes
(defctype result :int)
(defcenum %result
;; Return codes for successful operation execution (positive values)
  ;; Function successfully completed.
  (success 0)
  ;; The specified timeout time occurred before the operation could complete.
  (timeout-expired 1)
  ;; The session will be lost soon.
  (session-loss-pending 3)
  ;; No event was available.
  (event-unavailable 4)
  ;; The space's bounds are not known at the moment.
  (space-bounds-unavailable 7)
  ;; The session is not in the focused state.
  (session-not-focused 8)
  ;; A frame has been discarded from composition.
  (frame-discarded 9)
;; Error codes (negative values)
  ;; The function usage was invalid in some way.
  (error-validation-failure -1)
  ;; The runtime failed to handle the function in an unexpected way that is not covered by another error result.
  (error-runtime-failure -2)
  ;; A memory allocation has failed.
  (error-out-of-memory -3)
  ;; The runtime does not support the requested API version.
  (error-api-version-unsupported -4)
  ;; Initialization of object could not be completed.
  (error-initialization-failed -6)
  ;; The requested function was not found or is otherwise unsupported.
  (error-function-unsupported -7)
  ;; The requested feature is not supported.
  (error-feature-unsupported -8)
  ;; A requested extension is not supported.
  (error-extension-not-present -9)
  ;; The runtime supports no more of the requested resource.
  (error-limit-reached -10)
  ;; The supplied size was smaller than required.
  (error-size-insufficient -11)
  ;; A supplied object handle was invalid.
  (error-handle-invalid -12)
  ;; The slink:XrInstance was lost or could not be found. It will need to be destroyed and optionally recreated.
  (error-instance-lost -13)
  ;; The session <<session_running, is already running>>.
  (error-session-running -14)
  ;; The session <<session_not_running, is not yet running>>.
  (error-session-not-running -16)
  ;; The slink:XrSession was lost. It will need to be destroyed and optionally recreated.
  (error-session-lost -17)
  ;; The provided basetype:XrSystemId was invalid.
  (error-system-invalid -18)
  ;; The provided basetype:XrPath was not valid.
  (error-path-invalid -19)
  ;; The maximum number of supported semantic paths has been reached.
  (error-path-count-exceeded -20)
  ;; The semantic path character format is invalid.
  (error-path-format-invalid -21)
  ;; The semantic path is unsupported.
  (error-path-unsupported -22)
  ;; The layer was NULL or otherwise invalid.
  (error-layer-invalid -23)
  ;; The number of specified layers is greater than the supported number.
  (error-layer-limit-exceeded -24)
  ;; The image rect was negatively sized or otherwise invalid.
  (error-swapchain-rect-invalid -25)
  ;; The image format is not supported by the runtime or platform.
  (error-swapchain-format-unsupported -26)
  ;; The API used to retrieve an action's state does not match the action's type.
  (error-action-type-mismatch -27)
  ;; The session is not in the ready state.
  (error-session-not-ready -28)
  ;; The session is not in the stopping state.
  (error-session-not-stopping -29)
  ;; The provided basetype:XrTime was zero, negative, or out of range.
  (error-time-invalid -30)
  ;; The specified reference space is not supported by the runtime or system.
  (error-reference-space-unsupported -31)
  ;; The file could not be accessed.
  (error-file-access-error -32)
  ;; The file's contents were invalid.
  (error-file-contents-invalid -33)
  ;; The specified form factor is not supported by the current runtime or platform.
  (error-form-factor-unsupported -34)
  ;; The specified form factor is supported, but the device is currently not available, e.g. not plugged in or powered off.
  (error-form-factor-unavailable -35)
  ;; A requested API layer is not present or could not be loaded.
  (error-api-layer-not-present -36)
  ;; The call was made without having made a previously required call.
  (error-call-order-invalid -37)
  ;; The given graphics device is not in a valid state. The graphics device could be lost or initialized without meeting graphics requirements.
  (error-graphics-device-invalid -38)
  ;; The supplied pose was invalid with respect to the requirements.
  (error-pose-invalid -39)
  ;; The supplied index was outside the range of valid indices.
  (error-index-out-of-range -40)
  ;; The specified view configuration type is not supported by the runtime or platform.
  (error-view-configuration-type-unsupported -41)
  ;; The specified environment blend mode is not supported by the runtime or platform.
  (error-environment-blend-mode-unsupported -42)
  ;; The name provided was a duplicate of an already-existing resource.
  (error-name-duplicated -44)
  ;; The name provided was invalid.
  (error-name-invalid -45)
  ;; A referenced action set is not attached to the session.
  (error-actionset-not-attached -46)
  ;; The session already has attached action sets.
  (error-actionsets-already-attached -47)
  ;; The localized name provided was a duplicate of an already-existing resource.
  (error-localized-name-duplicated -48)
  ;; The localized name provided was invalid.
  (error-localized-name-invalid -49)
  ;; The fname:xrGetGraphicsRequirements* call was not made before calling fname:xrCreateSession.
  (error-graphics-requirements-call-missing -50)
  ;; The loader was unable to find or load a runtime.
  (error-runtime-unavailable -51)
;; xrSetAndroidApplicationThreadKHR failed as thread id is invalid.
  (error-android-thread-settings-id-invalid-khr -1000003000)
;; xrSetAndroidApplicationThreadKHR failed setting the thread attributes/priority.
  (error-android-thread-settings-failure-khr -1000003001)
;; Spatial anchor could not be created at that location.
  (error-create-spatial-anchor-failed-msft -1000039001)
;; The secondary view configuration was not enabled when creating the session.
  (error-secondary-view-configuration-type-not-enabled-msft -1000053000)
;; The controller model key is invalid.
  (error-controller-model-key-invalid-msft -1000055000)
;; The reprojection mode is not supported.
  (error-reprojection-mode-unsupported-msft -1000066000)
;; Compute new scene not completed.
  (error-compute-new-scene-not-completed-msft -1000097000)
;; Scene component id invalid.
  (error-scene-component-id-invalid-msft -1000097001)
;; Scene component type mismatch.
  (error-scene-component-type-mismatch-msft -1000097002)
;; Scene mesh buffer id invalid.
  (error-scene-mesh-buffer-id-invalid-msft -1000097003)
;; Scene compute feature incompatible.
  (error-scene-compute-feature-incompatible-msft -1000097004)
;; Scene compute consistency mismatch.
  (error-scene-compute-consistency-mismatch-msft -1000097005)
;; The display refresh rate is not supported by the platform.
  (error-display-refresh-rate-unsupported-fb -1000101000)
;; The color space is not supported by the runtime.
  (error-color-space-unsupported-fb -1000108000)
;; The component type is not supported for this space.
  (error-space-component-not-supported-fb -1000113000)
;; The required component is not enabled for this space.
  (error-space-component-not-enabled-fb -1000113001)
;; A request to set the component's status is currently pending.
  (error-space-component-status-pending-fb -1000113002)
;; The component is already set to the requested value.
  (error-space-component-status-already-set-fb -1000113003)
;; The object state is unexpected for the issued command.
  (error-unexpected-state-passthrough-fb -1000118000)
;; Trying to create an MR feature when one was already created and only one instance is allowed.
  (error-feature-already-created-passthrough-fb -1000118001)
;; Requested functionality requires a feature to be created first.
  (error-feature-required-passthrough-fb -1000118002)
;; Requested functionality is not permitted - application is not allowed to perform the requested operation.
  (error-not-permitted-passthrough-fb -1000118003)
;; There weren't sufficient resources available to perform an operation.
  (error-insufficient-resources-passthrough-fb -1000118004)
;; Unknown Passthrough error (no further details provided).
  (error-unknown-passthrough-fb -1000118050)
;; The model key is invalid.
  (error-render-model-key-invalid-fb -1000119000)
;; The model is unavailable.
  (nder-model-unavailable-fb 1000119020)
;; Marker tracking is disabled or the specified marker is not currently tracked.
  (error-marker-not-tracked-varjo -1000124000)
;; The specified marker ID is not valid.
  (error-marker-id-invalid-varjo -1000124001)
;; A spatial anchor was not found associated with the spatial anchor name provided
  (error-spatial-anchor-name-not-found-msft -1000142001)
;; The spatial anchor name provided was not valid
  (error-spatial-anchor-name-invalid-msft -1000142002)
;; Anchor import from cloud or export from device failed.
  (error-space-mapping-insufficient-fb -1000169000)
;; Anchors were downloaded from the cloud but failed to be imported/aligned on the device.
  (error-space-localization-failed-fb -1000169001)
;; Timeout occurred while waiting for network request to complete.
  (error-space-network-timeout-fb -1000169002)
;; The network request failed.
  (error-space-network-request-failed-fb -1000169003)
;; Cloud storage is required for this operation but is currently disabled.
  (error-space-cloud-storage-disabled-fb -1000169004)
;; Tracking optimization hint is already set for the domain.
  (error-hint-already-set-qcom -1000306000))

;; Enums to track objects of various types
(defcenum object-type
  (:unknown 0)
  ;; XrInstance
  (:instance 1)
  ;; XrSession
  (:session 2)
  ;; XrSwapchain
  (:swapchain 3)
  ;; XrSpace
  (:space 4)
  ;; XrActionSet
  (:action-set 5)
  ;; XrAction
  (:action 6)
;; XrDebugUtilsMessengerEXT
  (:debug-utils-messenger-ext 1000019000)
;; XrSpatialAnchorMSFT
  (:spatial-anchor-msft 1000039000)
;; XrSpatialGraphNodeBindingMSFT
  (:spatial-graph-node-binding-msft 1000049000)
;; XrHandTrackerEXT
  (:hand-tracker-ext 1000051000)
;; XrBodyTrackerFB
  (:body-tracker-fb 1000076000)
;; XrSceneObserverMSFT
  (:scene-observer-msft 1000097000)
;; XrSceneMSFT
  (:scene-msft 1000097001)
;; XrFacialTrackerHTC
  (:facial-tracker-htc 1000104000)
;; XrFoveationProfileFB
  (:foveation-profile-fb 1000114000)
;; XrTriangleMeshFB
  (:triangle-mesh-fb 1000117000)
;; XrPassthroughFB
  (:passthrough-fb 1000118000)
;; XrPassthroughLayerFB
  (:passthrough-layer-fb 1000118002)
;; XrGeometryInstanceFB
  (:geometry-instance-fb 1000118004)
;; XrSpatialAnchorStoreConnectionMSFT
  (:spatial-anchor-store-connection-msft 1000142000)
;; XrFaceTrackerFB
  (:face-tracker-fb 1000201000)
;; XrEyeTrackerFB
  (:eye-tracker-fb 1000202000)
;; XrSpaceUserFB
  (:space-user-fb 1000241000)
;; XrPassthroughHTC
  (:passthrough-htc 1000317000))

;; Android Thread Types
(defcenum android-thread-type-khr
  (:application-main-khr 1)
  (:application-worker-khr 2)
  (:renderer-main-khr 3)
  (:renderer-worker-khr 4))

;; eye visibility selector
(defcenum eye-visibility
  ;; Display in both eyes.
  (:both 0)
  ;; Display in the left eye only.
  (:left 1)
  ;; Display in the right eye only.
  (:right 2))

(defcenum action-type
  (:boolean-input 1)
  (:float-input 2)
  (:vector2f-input 3)
  (:pose-input 4)
  (:vibration-output 100))

(defcenum reference-space-type
  (:view 1)
  (:local 2)
  (:stage 3)
  (:unbounded-msft 1000038000)
  (:combined-eye-varjo 1000121000)
  (:local-floor-ext 1000426000))

(defcenum form-factor
  (:head-mounted-display 1)
  (:handheld-display 2))

(defcenum view-configuration-type
  (:primary-mono 1)
  (:primary-stereo 2)
  (:primary-quad-varjo 1000037000)
  (:secondary-mono-first-person-observer-msft 1000054000))

(defcenum environment-blend-mode
  (:opaque 1)
  (:additive 2)
  (:alpha-blend 3))

(defcenum session-state
  (:unknown 0)
  (:idle 1)
  (:ready 2)
  (:synchronized 3)
  (:visible 4)
  (:focused 5)
  (:stopping 6)
  (:loss-pending 7)
  (:exiting 8))

(defcenum perf-settings-domain-ext
  ;; Indicates that the performance settings or notification applies to CPU domain
  (:cpu-ext 1)
  ;; Indicates that the performance settings or notification applies to GPU domain
  (:gpu-ext 2))

(defcenum perf-settings-sub-domain-ext
  ;; Indicates that the performance notification originates from the COMPOSITING sub-domain
  (:compositing-ext 1)
  ;; Indicates that the performance notification originates from the RENDERING sub-domain
  (:rendering-ext 2)
  ;; Indicates that the performance notification originates from the THERMAL sub-domain
  (:thermal-ext 3))

;; enums for Perf setting EXT
(defcenum perf-settings-level-ext
  ;; Performance settings hint used by the application to indicate that it enters a non-XR section (head-locked / static screen), during which power savings are to be prioritized
  (:power-savings-ext 0)
  ;; Performance settings hint used by the application to indicate that it enters a low and stable complexity section, during which reducing power is more important than occasional late rendering frames
  (:sustained-low-ext 25)
  ;; Performance settings hint used by the application to indicate that it enters a high or dynamic complexity section, during which the XR Runtime strives for consistent XR compositing and frame rendering within a thermally sustainable range
  (:sustained-high-ext 50)
  ;; Performance settings hint used by the application to indicate that the application enters a section with very high complexity, during which the XR Runtime is allowed to step up beyond the thermally sustainable range
  (:boost-ext 75))

(defcenum perf-settings-notification-level-ext
  ;; Notifies that the sub-domain has reached a level where no further actions other than currently applied are necessary
  (:level-normal-ext 0)
  ;; Notifies that the sub-domain has reached an early warning level where the application should start proactive mitigation actions with the goal to return to the ename:XR_PERF_NOTIF_LEVEL_NORMAL level
  (:level-warning-ext 25)
  ;; Notifies that the sub-domain has reached a critical level with significant performance degradation. The application should take drastic mitigation action
  (:level-impaired-ext 75))

;; enums for XR_KHR_visibility_mask
;; enums for XR_KHR_visibility_mask
(defcenum visibility-mask-type-khr
  ;; exclusive mesh; indicates that which the viewer cannot see.
  (:hidden-triangle-mesh-khr 1)
  ;; inclusive mesh; indicates strictly that which the viewer can see.
  (:visible-triangle-mesh-khr 2)
  ;; line loop; traces the outline of the area the viewer can see.
  (:line-loop-khr 3))

;; enums for XR_MSFT_spatial_graph_bridge
;; enumes XR_MSFT_spatial_graph_bridge
(defcenum spatial-graph-node-type-msft
  (:static-msft 1)
  (:dynamic-msft 2))

;; enums for XR_FB_composition_layer_alpha_blend
;; XR_FB_composition_layer_alpha_blend
(defcenum blend-factor-fb
  (:zero-fb 0)
  (:one-fb 1)
  (:src-alpha-fb 2)
  (:one-minus-src-alpha-fb 3)
  (:dst-alpha-fb 4)
  (:one-minus-dst-alpha-fb 5))

;; enums for XR_FB_spatial_entity
;; enums for XR_FB_spatial_entity
(defcenum space-component-type-fb
  ;; Enables tracking the 6 DOF pose of the slink:XrSpace with flink:xrLocateSpace.
  (:locatable-fb 0)
  ;; Enables persistence operations: save and erase.
  (:storable-fb 1)
  ;; Enables sharing of spatial entities.
  (:sharable-fb 2)
  ;; Bounded 2D component.
  (:bounded-2d-fb 3)
  ;; Bounded 3D component.
  (:bounded-3d-fb 4)
  ;; Semantic labels component.
  (:semantic-labels-fb 5)
  ;; Room layout component.
  (:room-layout-fb 6)
  ;; Space container component.
  (:space-container-fb 7))

;; enums for XR_FB_triangle_mesh
(defcenum winding-order-fb
  ;; Winding order is unknown and the runtime cannot make any assumptions on the triangle orientation
  (:unknown-fb 0)
  ;; Clockwise winding order
  (:cw-fb 1)
  ;; Counter-clockwise winding order
  (:ccw-fb 2))

;; enums for XR_FB_passthrough
(defcenum passthrough-layer-purpose-fb
  ;; Reconstruction passthrough (full screen environment)
  (:reconstruction-fb 0)
  ;; Projected passthrough (using a custom surface)
  (:projected-fb 1)
;; Passthrough layer purpose for keyboard hands presence.
  (:tracked-keyboard-hands-fb 1000203001)
;; Passthrough layer purpose for keyboard hands presence with keyboard masked hand transitions (i.e passthrough hands rendered only when they are over the keyboard).
  (:tracked-keyboard-masked-hands-fb 1000203002))

;; enums for XR_FB_spatial_entity_query
;; enums for XR_FB_spatial_entity_query
(defcenum space-query-action-fb
  ;; Tells the query to perform a load operation on any slink:XrSpace returned by the query.
  (:load-fb 0))

;; enums for XR_FB_spatial_entity_storage
;; enums for XR_FB_spatial_entity_storage
(defcenum space-storage-location-fb
  ;; Invalid storage location
  (:invalid-fb 0)
  ;; Local device storage
  (:local-fb 1)
  ;; Cloud storage
  (:cloud-fb 2))

(defcenum space-persistence-mode-fb
  ;; Invalid storage persistence
  (:invalid-fb 0)
  ;; Store slink:XrSpace indefinitely, or until erased
  (:indefinite-fb 1))

(defcenum external-camera-attached-to-device-oculus
  ;; External camera is at a fixed point in LOCAL space
  (:none-oculus 0)
  ;; External camera is attached to the HMD
  (:hmd-oculus 1)
  ;; External camera is attached to a left Touch controller
  (:ltouch-oculus 2)
  ;; External camera is attached to a right Touch controller
  (:rtouch-oculus 3))

(defcenum performance-metrics-counter-unit-meta
  ;; the performance counter unit is generic (unspecified).
  (:generic-meta 0)
  ;; the performance counter unit is percentage (%).
  (:percentage-meta 1)
  ;; the performance counter unit is millisecond.
  (:illiseconds-meta 2)
  ;; the performance counter unit is byte.
  (:bytes-meta 3)
  ;; the performance counter unit is hertz (Hz).
  (:hertz-meta 4))

;; XR_HTC_facial_tracking
;; enums for XR_HTC_facial_tracking
(defcenum facial-tracking-type-htc
  ;; Specifies this handle will observe eye expressions, with values indexed by elink:XrEyeExpressionHTC whose count is dlink:XR_FACIAL_EXPRESSION_EYE_COUNT_HTC.
  (:eye-default-htc 1)
  ;; Specifies this handle will observe lip expressions, with values indexed by elink:XrLipExpressionHTC whose count is dlink:XR_FACIAL_EXPRESSION_LIP_COUNT_HTC.
  (:lip-default-htc 2))

(defcenum eye-expression-htc
  (:left-blink-htc 0)
  (:left-wide-htc 1)
  (:right-blink-htc 2)
  (:right-wide-htc 3)
  (:left-squeeze-htc 4)
  (:right-squeeze-htc 5)
  (:left-down-htc 6)
  (:right-down-htc 7)
  (:left-out-htc 8)
  (:right-in-htc 9)
  (:left-in-htc 10)
  (:right-out-htc 11)
  (:left-up-htc 12)
  (:right-up-htc 13))

(defcenum lip-expression-htc
  (:jaw-right-htc 0)
  (:jaw-left-htc 1)
  (:jaw-forward-htc 2)
  (:jaw-open-htc 3)
  (:mouth-ape-shape-htc 4)
  (:mouth-upper-right-htc 5)
  (:mouth-upper-left-htc 6)
  (:mouth-lower-right-htc 7)
  (:mouth-lower-left-htc 8)
  (:mouth-upper-overturn-htc 9)
  (:mouth-lower-overturn-htc 10)
  (:mouth-pout-htc 11)
  (:mouth-smile-right-htc 12)
  (:mouth-smile-left-htc 13)
  (:mouth-sad-right-htc 14)
  (:mouth-sad-left-htc 15)
  (:cheek-puff-right-htc 16)
  (:cheek-puff-left-htc 17)
  (:cheek-suck-htc 18)
  (:mouth-upper-upright-htc 19)
  (:mouth-upper-upleft-htc 20)
  (:mouth-lower-downright-htc 21)
  (:mouth-lower-downleft-htc 22)
  (:mouth-upper-inside-htc 23)
  (:mouth-lower-inside-htc 24)
  (:mouth-lower-overlay-htc 25)
  (:tongue-longstep1-htc 26)
  (:tongue-left-htc 27)
  (:tongue-right-htc 28)
  (:tongue-up-htc 29)
  (:tongue-down-htc 30)
  (:tongue-roll-htc 31)
  (:tongue-longstep2-htc 32)
  (:tongue-upright-morph-htc 33)
  (:tongue-upleft-morph-htc 34)
  (:tongue-downright-morph-htc 35)
  (:tongue-downleft-morph-htc 36))

;; enum for XR_HTC_passthrough
;; enums for XR_HTC_passthrough
(defcenum passthrough-form-htc
  ;; Presents the passthrough with full of the entire screen.
  (:planar-htc 0)
  ;; Presents the passthrough projecting onto a custom mesh.
  (:projected-htc 1))

;; enums for XR_HTC_foveation
;; enums for XR_HTC_foveation
(defcenum foveation-mode-htc
  ;; No foveation
  (:disable-htc 0)
  ;; Apply system default setting with fixed clear FOV and periphery quality.
  (:fixed-htc 1)
  ;; Allow system to set foveation dynamically according realtime system metric or other extensions.
  (:dynamic-htc 2)
  ;; Allow application to set foveation with desired clear FOV, periphery quality, and focal center offset.
  (:custom-htc 3))

(defcenum foveation-level-htc
  ;; No foveation
  (:none-htc 0)
  ;; Light periphery pixel density drop and lower performance gain.
  (:low-htc 1)
  ;; Medium periphery pixel density drop and medium performance gain
  (:medium-htc 2)
  ;; Heavy periphery pixel density drop and higher performance gain
  (:igh-htc 3))

;; XR_META_local_dimming
;; enums for XR_META_local_dimming
(defcenum local-dimming-mode-meta
  ;; Local dimming is turned off by default for the current submitted frame. This is the same as not chaining elink:XrLocalDimmingModeMETA.
  (:off-meta 0)
  ;; Local dimming is turned on for the current submitted frame.
  (:on-meta 1))

;; enums for XR_FB_face_tracking
;; XR_FB_face_tracking
(defcenum face-expression-fb
  (:brow-lowerer-l-fb 0)
  (:brow-lowerer-r-fb 1)
  (:cheek-puff-l-fb 2)
  (:cheek-puff-r-fb 3)
  (:cheek-raiser-l-fb 4)
  (:cheek-raiser-r-fb 5)
  (:cheek-suck-l-fb 6)
  (:cheek-suck-r-fb 7)
  (:chin-raiser-b-fb 8)
  (:chin-raiser-t-fb 9)
  (:dimpler-l-fb 10)
  (:dimpler-r-fb 11)
  (:eyes-closed-l-fb 12)
  (:eyes-closed-r-fb 13)
  (:eyes-look-down-l-fb 14)
  (:eyes-look-down-r-fb 15)
  (:eyes-look-left-l-fb 16)
  (:eyes-look-left-r-fb 17)
  (:eyes-look-right-l-fb 18)
  (:eyes-look-right-r-fb 19)
  (:eyes-look-up-l-fb 20)
  (:eyes-look-up-r-fb 21)
  (:inner-brow-raiser-l-fb 22)
  (:inner-brow-raiser-r-fb 23)
  (:jaw-drop-fb 24)
  (:jaw-sideways-left-fb 25)
  (:jaw-sideways-right-fb 26)
  (:jaw-thrust-fb 27)
  (:lid-tightener-l-fb 28)
  (:lid-tightener-r-fb 29)
  (:lip-corner-depressor-l-fb 30)
  (:lip-corner-depressor-r-fb 31)
  (:lip-corner-puller-l-fb 32)
  (:lip-corner-puller-r-fb 33)
  (:lip-funneler-lb-fb 34)
  (:lip-funneler-lt-fb 35)
  (:lip-funneler-rb-fb 36)
  (:lip-funneler-rt-fb 37)
  (:lip-pressor-l-fb 38)
  (:lip-pressor-r-fb 39)
  (:lip-pucker-l-fb 40)
  (:lip-pucker-r-fb 41)
  (:lip-stretcher-l-fb 42)
  (:lip-stretcher-r-fb 43)
  (:lip-suck-lb-fb 44)
  (:lip-suck-lt-fb 45)
  (:lip-suck-rb-fb 46)
  (:lip-suck-rt-fb 47)
  (:lip-tightener-l-fb 48)
  (:lip-tightener-r-fb 49)
  (:lips-toward-fb 50)
  (:lower-lip-depressor-l-fb 51)
  (:lower-lip-depressor-r-fb 52)
  (:mouth-left-fb 53)
  (:mouth-right-fb 54)
  (:nose-wrinkler-l-fb 55)
  (:nose-wrinkler-r-fb 56)
  (:outer-brow-raiser-l-fb 57)
  (:outer-brow-raiser-r-fb 58)
  (:upper-lid-raiser-l-fb 59)
  (:upper-lid-raiser-r-fb 60)
  (:upper-lip-raiser-l-fb 61)
  (:upper-lip-raiser-r-fb 62)
  (:count-fb 63))

(defcenum face-expression-set-fb
  ;; indicates that the created slink:XrFaceTrackerFB tracks the set of blend shapes described by elink:XrFaceExpressionFB enum, i.e. the flink:xrGetFaceExpressionWeightsFB function returns an array of blend shapes with the count of ename:XR_FACE_EXPRESSION_COUNT_FB and can: be indexed using elink:XrFaceExpressionFB.
  (:default-fb 0))

(defcenum face-confidence-fb
  (:lower-face-fb 0)
  (:upper-face-fb 1)
  (:count-fb 2))

;; enums for XR_FB_body_tracking
;; XR_FB_body_tracking
(defcenum body-joint-fb
  (:root-fb 0)
  (:hips-fb 1)
  (:spine-lower-fb 2)
  (:spine-middle-fb 3)
  (:spine-upper-fb 4)
  (:chest-fb 5)
  (:neck-fb 6)
  (:head-fb 7)
  (:left-shoulder-fb 8)
  (:left-scapula-fb 9)
  (:left-arm-upper-fb 10)
  (:left-arm-lower-fb 11)
  (:left-hand-wrist-twist-fb 12)
  (:right-shoulder-fb 13)
  (:right-scapula-fb 14)
  (:right-arm-upper-fb 15)
  (:right-arm-lower-fb 16)
  (:right-hand-wrist-twist-fb 17)
  (:left-hand-palm-fb 18)
  (:left-hand-wrist-fb 19)
  (:left-hand-thumb-metacarpal-fb 20)
  (:left-hand-thumb-proximal-fb 21)
  (:left-hand-thumb-distal-fb 22)
  (:left-hand-thumb-tip-fb 23)
  (:left-hand-index-metacarpal-fb 24)
  (:left-hand-index-proximal-fb 25)
  (:left-hand-index-intermediate-fb 26)
  (:left-hand-index-distal-fb 27)
  (:left-hand-index-tip-fb 28)
  (:left-hand-middle-metacarpal-fb 29)
  (:left-hand-middle-proximal-fb 30)
  (:left-hand-middle-intermediate-fb 31)
  (:left-hand-middle-distal-fb 32)
  (:left-hand-middle-tip-fb 33)
  (:left-hand-ring-metacarpal-fb 34)
  (:left-hand-ring-proximal-fb 35)
  (:left-hand-ring-intermediate-fb 36)
  (:left-hand-ring-distal-fb 37)
  (:left-hand-ring-tip-fb 38)
  (:left-hand-little-metacarpal-fb 39)
  (:left-hand-little-proximal-fb 40)
  (:left-hand-little-intermediate-fb 41)
  (:left-hand-little-distal-fb 42)
  (:left-hand-little-tip-fb 43)
  (:right-hand-palm-fb 44)
  (:right-hand-wrist-fb 45)
  (:right-hand-thumb-metacarpal-fb 46)
  (:right-hand-thumb-proximal-fb 47)
  (:right-hand-thumb-distal-fb 48)
  (:right-hand-thumb-tip-fb 49)
  (:right-hand-index-metacarpal-fb 50)
  (:right-hand-index-proximal-fb 51)
  (:right-hand-index-intermediate-fb 52)
  (:right-hand-index-distal-fb 53)
  (:right-hand-index-tip-fb 54)
  (:right-hand-middle-metacarpal-fb 55)
  (:right-hand-middle-proximal-fb 56)
  (:right-hand-middle-intermediate-fb 57)
  (:right-hand-middle-distal-fb 58)
  (:right-hand-middle-tip-fb 59)
  (:right-hand-ring-metacarpal-fb 60)
  (:right-hand-ring-proximal-fb 61)
  (:right-hand-ring-intermediate-fb 62)
  (:right-hand-ring-distal-fb 63)
  (:right-hand-ring-tip-fb 64)
  (:right-hand-little-metacarpal-fb 65)
  (:right-hand-little-proximal-fb 66)
  (:right-hand-little-intermediate-fb 67)
  (:right-hand-little-distal-fb 68)
  (:right-hand-little-tip-fb 69)
  (:count-fb 70)
  (:none-fb -1))

;; Describes the set of body joints to track when creating an slink:XrBodyTrackerFB.
(defcenum body-joint-set-fb
  ;; Indicates that the created slink:XrBodyTrackerFB tracks the set of body joints described by elink:XrBodyJointFB enum, i.e. the flink:xrLocateBodyJointsFB function returns an array of joint locations with the count of ename:XR_BODY_JOINT_COUNT_FB and can be indexed using elink:XrBodyJointFB.
  (:default-fb 0))

;; enums for XR_FB_eye_tracking_social
;; XR_FB_eye_tracking_social
(defcenum eye-position-fb
  ;; Specifies the position of the left eye.
  (:left-fb 0)
  ;; Specifies the position of the right eye.
  (:right-fb 1)
  (:count-fb 2))

;; enums for XR_QCOM_tracking_optimization_settings extension
(defcenum tracking-optimization-settings-domain-qcom
  ;; Setting applies to all QCOM tracking extensions.
  (:all-qcom 1))

;; enums for XR_QCOM_tracking_optimization_settings
(defcenum tracking-optimization-settings-hint-qcom
  ;; Used by the application to indicate that it does not have a preference to optimize for. The run-time is understood to choose a balanced approach.
  (:none-qcom 0)
  ;; Used by the application to indicate that it prefers tracking to be optimized for long range, possibly at the expense of competing interests.
  (:long-range-priorization-qcom 1)
  ;; Used by the application to indicate that it prefers tracking to be optimized for close range, possibly at the expense of competing interests.
  (:close-range-priorization-qcom 2)
  ;; Used by the application to indicate that it prefers tracking to be optimized for low power consumption, possibly at the expense of competing interests.
  (:low-power-priorization-qcom 3)
  ;; Used by the application to indicate that it prefers tracking to be optimized for increased tracking performance, possibly at the cost of increased power consumption.
  (:high-power-priorization-qcom 4))

;; XR_MNDX_force_feedback_curl
;; enums for XR_MNDX_force_feedback_curl
(defcenum force-feedback-curl-location-mndx
  ;; force feedback for thumb curl
  (:thumb-curl-mndx 0)
  ;; force feedback for index finger curl
  (:index-curl-mndx 1)
  ;; force feedback for middle finger curl
  (:iddle-curl-mndx 2)
  ;; force feedback for ring finger curl
  (:ring-curl-mndx 3)
  ;; force feedback for little finger curl
  (:little-curl-mndx 4))

;; The PFN_xrVoidFunction type are used by XrGetInstanceProcAddr below
;; typedef void (XRAPI_PTR *PFN_xrVoidFunction)(void);
(defctype pfn-void-function :pointer)

;; The PFN_xrDebugUtilsMessengerCallbackEXT type are used by the XR_EXT_debug_utils extension
;; typedef XrBool32 (XRAPI_PTR *PFN_xrDebugUtilsMessengerCallbackEXT)(
;;             XrDebugUtilsMessageSeverityFlagsEXT              messageSeverity,
;;             XrDebugUtilsMessageTypeFlagsEXT                  messageTypes,
;;             const XrDebugUtilsMessengerCallbackDataEXT*      callbackData,
;;             void*                                            userData);
(defctype pfn-debug-utils-messenger-callback-ext :pointer)

;; types for XR_MSFT_spatial_graph_bridge
(defctype spatial-graph-node-binding-msft xr-handle)

;; XR_EXT_hand_tracking
;; XR_EXT_hand_tracking
(defcenum hand-ext
  (:left-ext 1)
  (:right-ext 2))

(defcenum hand-joint-ext
  (:palm-ext 0)
  (:wrist-ext 1)
  (:thumb-metacarpal-ext 2)
  (:thumb-proximal-ext 3)
  (:thumb-distal-ext 4)
  (:thumb-tip-ext 5)
  (:index-metacarpal-ext 6)
  (:index-proximal-ext 7)
  (:index-intermediate-ext 8)
  (:index-distal-ext 9)
  (:index-tip-ext 10)
  (:middle-metacarpal-ext 11)
  (:middle-proximal-ext 12)
  (:middle-intermediate-ext 13)
  (:middle-distal-ext 14)
  (:middle-tip-ext 15)
  (:ring-metacarpal-ext 16)
  (:ring-proximal-ext 17)
  (:ring-intermediate-ext 18)
  (:ring-distal-ext 19)
  (:ring-tip-ext 20)
  (:little-metacarpal-ext 21)
  (:little-proximal-ext 22)
  (:little-intermediate-ext 23)
  (:little-distal-ext 24)
  (:little-tip-ext 25))

(defcenum hand-joint-set-ext
  (:default-ext 0)
  (:hand-with-forearm-ultraleap 1000149000))

;; XR_EXT_controller_hand_joints_motion_range
;; XR_EXT_hand_joints_motion_range
(defcenum hand-joints-motion-range-ext
  (:unobstructed-ext 1)
  (:conforming-to-controller-ext 2))

;; XR_MSFT_hand_tracking_mesh
;; XR_MSFT_hand_tracking_mesh
(defcenum hand-pose-type-msft
  (:tracked-msft 0)
  (:reference-open-palm-msft 1))

;; XR_MSFT_scene_understanding
(defctype scene-observer-msft xr-handle)

(defctype scene-msft xr-handle)

;; enumes XR_MSFT_scene_understanding
(defcenum scene-object-type-msft
  (:uncategorized-msft -1)
  (:background-msft 1)
  (:wall-msft 2)
  (:floor-msft 3)
  (:ceiling-msft 4)
  (:platform-msft 5)
  (:inferred-msft 6))

(defcenum scene-plane-alignment-type-msft
  (:non-orthogonal-msft 0)
  (:horizontal-msft 1)
  (:vertical-msft 2))

(defcenum scene-compute-state-msft
  (:none-msft 0)
  (:updating-msft 1)
  (:completed-msft 2)
  (:completed-with-error-msft 3))

(defcenum scene-compute-feature-msft
  (:plane-msft 1)
  (:plane-mesh-msft 2)
  (:visual-mesh-msft 3)
  (:collider-mesh-msft 4)
  (:serialize-scene-msft 1000098000))

(defcenum scene-compute-consistency-msft
  (:snapshot-complete-msft 1)
  (:snapshot-incomplete-fast-msft 2)
  (:occlusion-optimized-msft 3))

(defcenum scene-component-type-msft
  (:invalid-msft -1)
  (:object-msft 1)
  (:plane-msft 2)
  (:visual-mesh-msft 3)
  (:collider-mesh-msft 4)
  (:serialized-scene-fragment-msft 1000098000))

(defcenum mesh-compute-lod-msft
  (:coarse-msft 1)
  (:edium-msft 2)
  (:fine-msft 3)
  (:unlimited-msft 4))

;; XR_FB_color_space structs
;; XR_FB_color_space
(defcenum color-space-fb
  (:unmanaged-fb 0)
  (:rec2020-fb 1)
  (:rec709-fb 2)
  (:rift-cv1-fb 3)
  (:rift-s-fb 4)
  (:quest-fb 5)
  (:p3-fb 6)
  (:adobe-rgb-fb 7))

;; XR_FB_foveation_configuration structs
;; enums for XR_FB_foveation_configuration
(defcenum foveation-level-fb
  ;; No foveation
  (:none-fb 0)
  ;; Less foveation (higher periphery visual fidelity, lower performance)
  (:low-fb 1)
  ;; Medium foveation (medium periphery visual fidelity, medium performance)
  (:medium-fb 2)
  ;; High foveation (lower periphery visual fidelity, higher performance)
  (:high-fb 3))

(defcenum foveation-dynamic-fb
  ;; Static foveation at the maximum desired level
  (:disabled-fb 0)
  ;; Dynamic changing foveation based on performance headroom available up to the maximum desired level
  (:level-enabled-fb 1))

;; XR_MSFT_composition_layer_reprojection
;; XR_MSFT_composition_layer_reprojection
(defcenum reprojection-mode-msft
  (:depth-msft 1)
  (:planar-from-depth-msft 2)
  (:planar-manual-msft 3)
  (:orientation-only-msft 4))

;; XR_MSFT_spatial_anchor_persistence
(defctype spatial-anchor-store-connection-msft xr-handle)

;; XR_ULTRALEAP_hand_tracking_forearm
;; XR_ULTRALEAP_hand_tracking_forearm
(defcenum hand-forearm-joint-ultraleap
  (:palm-ultraleap 0)
  (:wrist-ultraleap 1)
  (:thumb-metacarpal-ultraleap 2)
  (:thumb-proximal-ultraleap 3)
  (:thumb-distal-ultraleap 4)
  (:thumb-tip-ultraleap 5)
  (:index-metacarpal-ultraleap 6)
  (:index-proximal-ultraleap 7)
  (:index-intermediate-ultraleap 8)
  (:index-distal-ultraleap 9)
  (:index-tip-ultraleap 10)
  (:middle-metacarpal-ultraleap 11)
  (:middle-proximal-ultraleap 12)
  (:middle-intermediate-ultraleap 13)
  (:middle-distal-ultraleap 14)
  (:middle-tip-ultraleap 15)
  (:ring-metacarpal-ultraleap 16)
  (:ring-proximal-ultraleap 17)
  (:ring-intermediate-ultraleap 18)
  (:ring-distal-ultraleap 19)
  (:ring-tip-ultraleap 20)
  (:little-metacarpal-ultraleap 21)
  (:little-proximal-ultraleap 22)
  (:little-intermediate-ultraleap 23)
  (:little-distal-ultraleap 24)
  (:little-tip-ultraleap 25)
  (:elbow-ultraleap 26))

;; XR_FB_composition_layer_depth_test
;; enums for XR_FB_composition_layer_depth_test
(defcenum compare-op-fb
  ;; Comparison is never true.
  (:never-fb 0)
  ;; Comparison is true if source less than is destination.
  (:less-fb 1)
  ;; Comparison is true if source is equal to destination.
  (:equal-fb 2)
  ;; Comparison is true if source is less than or equal to destination.
  (:less-or-equal-fb 3)
  ;; Comparison is true if source is greater than destination.
  (:greater-fb 4)
  ;; Comparison is true if source is not equal to destination.
  (:not-equal-fb 5)
  ;; Comparison is true if source is greater than or equal to destination.
  (:greater-or-equal-fb 6)
  ;; Comparison is always true.
  (:always-fb 7))

;; Struct types
(defcstruct vector-2f
  (x :float)
  (y :float))

(defcstruct vector-3f
  (x :float)
  (y :float)
  (z :float))

(defcstruct vector-4f
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defcstruct color-4f
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(defcstruct quaternion-f
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defcstruct pose-f
  (orientation (:struct quaternion-f))
  (position (:struct vector-3f)))

(defcstruct offset-2d-f
  (x :float)
  (y :float))

(defcstruct extent-2d-f
  (width :float)
  (height :float))

(defcstruct rect-2d-f
  (offset (:struct offset-2d-f))
  (extent (:struct extent-2d-f)))

(defcstruct offset-2d-i
  (x :int32)
  (y :int32))

(defcstruct extent-2d-i
  (width :int32)
  (height :int32))

(defcstruct rect-2d-i
  (offset (:struct offset-2d-i))
  (extent (:struct extent-2d-i)))

;;  mayalias : true
(defcstruct base-in-structure
  (type structure-type)
  (next (:pointer (:struct base-in-structure))))

;;  mayalias : true
(defcstruct base-out-structure
  (type structure-type)
  (next (:pointer (:struct base-out-structure))))

;;  returned only : true
(defcstruct api-layer-properties
  ;; values = :TYPE-API-LAYER-PROPERTIES
  (type structure-type)
  (next (:pointer :void))
  ;; length = +MAX-API-LAYER-NAME-SIZE+
  (layer-name :char :count 256)
  (spec-version version)
  (layer-version :uint32)
  ;; length = +MAX-API-LAYER-DESCRIPTION-SIZE+
  (description :char :count 256))

;;  returned only : true
(defcstruct extension-properties
  ;; values = :TYPE-EXTENSION-PROPERTIES
  (type structure-type)
  (next (:pointer :void))
  ;; length = +MAX-EXTENSION-NAME-SIZE+
  (extension-name :char :count 128)
  (extension-version :uint32))

(defcstruct application-info
  ;; length = +MAX-APPLICATION-NAME-SIZE+
  (application-name :char :count 128)
  (application-version :uint32)
  ;; length = +MAX-ENGINE-NAME-SIZE+
  (engine-name :char :count 128)
  (engine-version :uint32)
  (api-version version))

(defcstruct instance-create-info
  ;; values = :TYPE-INSTANCE-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (create-flags instance-create-flags) ;; optional
  (application-info (:struct application-info))
  (enabled-api-layer-count :uint32) ;; optional
  ;; length = enabled-api-layer-count :null-terminated
  (enabled-api-layer-names (:pointer :string))
  (enabled-extension-count :uint32) ;; optional
  ;; length = enabled-extension-count :null-terminated
  (enabled-extension-names (:pointer :string)))

;;  returned only : true
(defcstruct instance-properties
  ;; values = :TYPE-INSTANCE-PROPERTIES
  (type structure-type)
  (next (:pointer :void))
  (runtime-version version)
  ;; length = +MAX-RUNTIME-NAME-SIZE+
  (runtime-name :char :count 128))

(defcstruct system-get-info
  ;; values = :TYPE-SYSTEM-GET-INFO
  (type structure-type)
  (next (:pointer :void))
  (form-factor form-factor))

(defcstruct system-tracking-properties
  (orientation-tracking bool-32)
  (position-tracking bool-32))

(defcstruct system-graphics-properties
  (max-swapchain-image-height :uint32)
  (max-swapchain-image-width :uint32)
  (max-layer-count :uint32))

;;  returned only : true
(defcstruct system-properties
  ;; values = :TYPE-SYSTEM-PROPERTIES
  (type structure-type)
  (next (:pointer :void))
  (system-id system-id)
  (vendor-id :uint32)
  ;; length = +MAX-SYSTEM-NAME-SIZE+
  (system-name :char :count 256)
  (graphics-properties (:struct system-graphics-properties))
  (tracking-properties (:struct system-tracking-properties)))

;;  extends : SESSION-CREATE-INFO
;;  protect : XR_USE_PLATFORM_WIN32
(defcstruct graphics-binding-opengl-win32-khr
  ;; values = :TYPE-GRAPHICS-BINDING-OPENGL-WIN32-KHR
  (type structure-type)
  (next (:pointer :void))
  (hdc hdc)
  (hglrc hglrc))

;;  extends : SESSION-CREATE-INFO
;;  protect : XR_USE_PLATFORM_XLIB
(defcstruct graphics-binding-opengl-xlib-khr
  ;; values = :TYPE-GRAPHICS-BINDING-OPENGL-XLIB-KHR
  (type structure-type)
  (next (:pointer :void))
  (x-display (:pointer display))
  (visualid :uint32)
  (glx-fbconfig glx-fb-config)
  (glx-drawable glx-drawable)
  (glx-context glx-context))

;;  extends : SESSION-CREATE-INFO
;;  protect : XR_USE_PLATFORM_XCB
(defcstruct graphics-binding-opengl-xcb-khr
  ;; values = :TYPE-GRAPHICS-BINDING-OPENGL-XCB-KHR
  (type structure-type)
  (next (:pointer :void))
  (connection (:pointer xcb-connection-t))
  (screen-number :uint32)
  (fbconfigid xcb-glx-fbconfig-t)
  (visualid xcb-visualid-t)
  (glx-drawable xcb-glx-drawable-t)
  (glx-context xcb-glx-context-t))

;;  extends : SESSION-CREATE-INFO
;;  protect : XR_USE_PLATFORM_WAYLAND
(defcstruct graphics-binding-opengl-wayland-khr
  ;; values = :TYPE-GRAPHICS-BINDING-OPENGL-WAYLAND-KHR
  (type structure-type)
  (next (:pointer :void))
  (display (:pointer wl-display)))

;;  extends : SESSION-CREATE-INFO
(defcstruct graphics-binding-d3d11-khr
  ;; values = :TYPE-GRAPHICS-BINDING-D3D11-KHR
  (type structure-type)
  (next (:pointer :void))
  (device (:pointer i-d3d11-device)))

;;  extends : SESSION-CREATE-INFO
(defcstruct graphics-binding-d3d12-khr
  ;; values = :TYPE-GRAPHICS-BINDING-D3D12-KHR
  (type structure-type)
  (next (:pointer :void))
  (device (:pointer i-d3d12-device))
  (queue (:pointer i-d3d12-command-queue)))

;;  extends : SESSION-CREATE-INFO
;;  protect : XR_USE_PLATFORM_ANDROID
(defcstruct graphics-binding-opengl-es-android-khr
  ;; values = :TYPE-GRAPHICS-BINDING-OPENGL-ES-ANDROID-KHR
  (type structure-type)
  (next (:pointer :void))
  (display egl-display)
  (config egl-config)
  (context egl-context))

;;  extends : SESSION-CREATE-INFO
(defcstruct graphics-binding-vulkan-khr
  ;; values = :TYPE-GRAPHICS-BINDING-VULKAN-KHR
  (type structure-type)
  (next (:pointer :void))
  (instance vk-image)
  (physical-device vk-physical-device)
  (device vk-device)
  (queue-family-index :uint32)
  (queue-index :uint32))

(defcstruct session-create-info
  ;; values = :TYPE-SESSION-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (create-flags session-create-flags) ;; optional
  (system-id system-id))

(defcstruct session-begin-info
  ;; values = :TYPE-SESSION-BEGIN-INFO
  (type structure-type)
  (next (:pointer :void))
  (primary-view-configuration-type view-configuration-type))

(defcstruct swapchain-create-info
  ;; values = :TYPE-SWAPCHAIN-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (create-flags swapchain-create-flags) ;; optional
  (usage-flags swapchain-usage-flags) ;; optional
  (format :int64)
  (sample-count :uint32)
  (width :uint32)
  (height :uint32)
  (face-count :uint32)
  (array-size :uint32)
  (mip-count :uint32))

;;  returned only : true
(defcstruct swapchain-image-base-header
  (type structure-type)
  (next (:pointer :void)))

;;  parent : SWAPCHAIN-IMAGE-BASE-HEADER
;;  returned only : true
(defcstruct swapchain-image-opengl-khr
  ;; values = :TYPE-SWAPCHAIN-IMAGE-OPENGL-KHR
  (type structure-type)
  (next (:pointer :void))
  (image :uint32))

;;  parent : SWAPCHAIN-IMAGE-BASE-HEADER
;;  returned only : true
(defcstruct swapchain-image-opengl-es-khr
  ;; values = :TYPE-SWAPCHAIN-IMAGE-OPENGL-ES-KHR
  (type structure-type)
  (next (:pointer :void))
  (image :uint32))

;;  parent : SWAPCHAIN-IMAGE-BASE-HEADER
;;  returned only : true
(defcstruct swapchain-image-vulkan-khr
  ;; values = :TYPE-SWAPCHAIN-IMAGE-VULKAN-KHR
  (type structure-type)
  (next (:pointer :void))
  (image vk-image))

;;  parent : SWAPCHAIN-IMAGE-BASE-HEADER
;;  returned only : true
(defcstruct swapchain-image-d3d11-khr
  ;; values = :TYPE-SWAPCHAIN-IMAGE-D3D11-KHR
  (type structure-type)
  (next (:pointer :void))
  (texture (:pointer i-d3d11-texture-2d)))

;;  parent : SWAPCHAIN-IMAGE-BASE-HEADER
;;  returned only : true
(defcstruct swapchain-image-d3d12-khr
  ;; values = :TYPE-SWAPCHAIN-IMAGE-D3D12-KHR
  (type structure-type)
  (next (:pointer :void))
  (texture (:pointer i-d3d12-resource)))

(defcstruct swapchain-image-acquire-info
  ;; values = :TYPE-SWAPCHAIN-IMAGE-ACQUIRE-INFO
  (type structure-type)
  (next (:pointer :void)))

(defcstruct swapchain-image-wait-info
  ;; values = :TYPE-SWAPCHAIN-IMAGE-WAIT-INFO
  (type structure-type)
  (next (:pointer :void))
  (timeout duration))

(defcstruct swapchain-image-release-info
  ;; values = :TYPE-SWAPCHAIN-IMAGE-RELEASE-INFO
  (type structure-type)
  (next (:pointer :void)))

(defcstruct reference-space-create-info
  ;; values = :TYPE-REFERENCE-SPACE-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (reference-space-type reference-space-type)
  (pose-in-reference-space (:struct pose-f)))

(defcstruct action-space-create-info
  ;; values = :TYPE-ACTION-SPACE-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (action action)
  (subaction-path path) ;; optional
  (pose-in-action-space (:struct pose-f)))

(defcstruct space-location
  ;; values = :TYPE-SPACE-LOCATION
  (type structure-type)
  (next (:pointer :void))
  (location-flags space-location-flags) ;; optional
  (pose (:struct pose-f)))

;;  extends : SPACE-LOCATION
(defcstruct space-velocity
  ;; values = :TYPE-SPACE-VELOCITY
  (type structure-type)
  (next (:pointer :void))
  (velocity-flags space-velocity-flags) ;; optional
  (linear-velocity (:struct vector-3f))
  (angular-velocity (:struct vector-3f)))

(defcstruct fov-f
  (angle-left :float)
  (angle-right :float)
  (angle-up :float)
  (angle-down :float))

(defcstruct view
  ;; values = :TYPE-VIEW
  (type structure-type)
  (next (:pointer :void))
  (pose (:struct pose-f))
  (fov (:struct fov-f)))

(defcstruct view-locate-info
  ;; values = :TYPE-VIEW-LOCATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-type view-configuration-type)
  (display-time time)
  (space space))

(defcstruct view-state
  ;; values = :TYPE-VIEW-STATE
  (type structure-type)
  (next (:pointer :void))
  (view-state-flags view-state-flags) ;; optional
)

(defcstruct view-configuration-view
  ;; values = :TYPE-VIEW-CONFIGURATION-VIEW
  (type structure-type)
  (next (:pointer :void))
  (recommended-image-rect-width :uint32)
  (max-image-rect-width :uint32)
  (recommended-image-rect-height :uint32)
  (max-image-rect-height :uint32)
  (recommended-swapchain-sample-count :uint32)
  (max-swapchain-sample-count :uint32))

(defcstruct swapchain-sub-image
  (swapchain swapchain)
  (image-rect (:struct rect-2d-i))
  (image-array-index :uint32))

(defcstruct composition-layer-base-header
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space))

(defcstruct composition-layer-projection-view
  ;; values = :TYPE-COMPOSITION-LAYER-PROJECTION-VIEW
  (type structure-type)
  (next (:pointer :void))
  (pose (:struct pose-f))
  (fov (:struct fov-f))
  (sub-image (:struct swapchain-sub-image)))

;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-projection
  ;; values = :TYPE-COMPOSITION-LAYER-PROJECTION
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (view-count :uint32)
  ;; length = view-count
  (views (:pointer (:struct composition-layer-projection-view))))

;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-quad
  ;; values = :TYPE-COMPOSITION-LAYER-QUAD
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (size (:struct extent-2d-f)))

;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-cylinder-khr
  ;; values = :TYPE-COMPOSITION-LAYER-CYLINDER-KHR
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (radius :float)
  (central-angle :float)
  (aspect-ratio :float))

;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-cube-khr
  ;; values = :TYPE-COMPOSITION-LAYER-CUBE-KHR
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (swapchain swapchain)
  (image-array-index :uint32)
  (orientation (:struct quaternion-f)))

;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-equirect-khr
  ;; values = :TYPE-COMPOSITION-LAYER-EQUIRECT-KHR
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (radius :float)
  (scale (:struct vector-2f))
  (bias (:struct vector-2f)))

;;  extends : COMPOSITION-LAYER-PROJECTION-VIEW
(defcstruct composition-layer-depth-info-khr
  ;; values = :TYPE-COMPOSITION-LAYER-DEPTH-INFO-KHR
  (type structure-type)
  (next (:pointer :void))
  (sub-image (:struct swapchain-sub-image))
  (min-depth :float)
  (max-depth :float)
  (near-z :float)
  (far-z :float))

(defcstruct frame-begin-info
  ;; values = :TYPE-FRAME-BEGIN-INFO
  (type structure-type)
  (next (:pointer :void)))

(defcstruct frame-end-info
  ;; values = :TYPE-FRAME-END-INFO
  (type structure-type)
  (next (:pointer :void))
  (display-time time)
  (environment-blend-mode environment-blend-mode)
  (layer-count :uint32) ;; optional
  ;; length = layer-count
  (layers (:pointer (:pointer (:struct composition-layer-base-header)))) ;; optional
)

(defcstruct frame-wait-info
  ;; values = :TYPE-FRAME-WAIT-INFO
  (type structure-type)
  (next (:pointer :void)))

(defcstruct frame-state
  ;; values = :TYPE-FRAME-STATE
  (type structure-type)
  (next (:pointer :void))
  (predicted-display-time time)
  (predicted-display-period duration)
  (should-render bool-32))

(defcstruct haptic-base-header
  (type structure-type)
  (next (:pointer :void)))

;;  parent : HAPTIC-BASE-HEADER
(defcstruct haptic-vibration
  ;; values = :TYPE-HAPTIC-VIBRATION
  (type structure-type)
  (next (:pointer :void))
  (duration duration)
  (frequency :float) ;; optional
  (amplitude :float))

;;  returned only : true
(defcstruct event-data-base-header
;; top-level, parentstruct="XrBaseOutStructure" causes validation failures
  (type structure-type)
  (next (:pointer :void)))

(defcstruct event-data-buffer
;; top-level, parentstruct="XrBaseInStructure" causes validation failures
  ;; values = :TYPE-EVENT-DATA-BUFFER
  (type structure-type)
  (next (:pointer :void))
  (varying :uint8 :count 4000))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-events-lost
  ;; values = :TYPE-EVENT-DATA-EVENTS-LOST
  (type structure-type)
  (next (:pointer :void))
  (lost-event-count :uint32))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-instance-loss-pending
  ;; values = :TYPE-EVENT-DATA-INSTANCE-LOSS-PENDING
  (type structure-type)
  (next (:pointer :void))
  (loss-time time))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-session-state-changed
  ;; values = :TYPE-EVENT-DATA-SESSION-STATE-CHANGED
  (type structure-type)
  (next (:pointer :void))
  (session session)
  (state session-state)
  (time time))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-reference-space-change-pending
  ;; values = :TYPE-EVENT-DATA-REFERENCE-SPACE-CHANGE-PENDING
  (type structure-type)
  (next (:pointer :void))
  (session session)
  (reference-space-type reference-space-type)
  (change-time time)
  (pose-valid bool-32)
  (pose-in-previous-space (:struct pose-f)))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-perf-settings-ext
  ;; values = :TYPE-EVENT-DATA-PERF-SETTINGS-EXT
  (type structure-type)
  (next (:pointer :void))
  (domain perf-settings-domain-ext)
  (sub-domain perf-settings-sub-domain-ext)
  (from-level perf-settings-notification-level-ext)
  (to-level perf-settings-notification-level-ext))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-visibility-mask-changed-khr
  ;; values = :TYPE-EVENT-DATA-VISIBILITY-MASK-CHANGED-KHR
  (type structure-type)
  (next (:pointer :void))
  (session session)
  (view-configuration-type view-configuration-type)
  (view-index :uint32))

(defcstruct view-configuration-properties
  ;; values = :TYPE-VIEW-CONFIGURATION-PROPERTIES
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-type view-configuration-type)
  (fov-mutable bool-32))

(defcstruct action-state-boolean
  ;; values = :TYPE-ACTION-STATE-BOOLEAN
  (type structure-type)
  (next (:pointer :void))
  (current-state bool-32)
  (changed-since-last-sync bool-32)
  (last-change-time time)
  (is-active bool-32))

(defcstruct action-state-float
  ;; values = :TYPE-ACTION-STATE-FLOAT
  (type structure-type)
  (next (:pointer :void))
  (current-state :float)
  (changed-since-last-sync bool-32)
  (last-change-time time)
  (is-active bool-32))

(defcstruct action-state-vector-2f
  ;; values = :TYPE-ACTION-STATE-VECTOR2F
  (type structure-type)
  (next (:pointer :void))
  (current-state (:struct vector-2f))
  (changed-since-last-sync bool-32)
  (last-change-time time)
  (is-active bool-32))

(defcstruct action-state-pose
  ;; values = :TYPE-ACTION-STATE-POSE
  (type structure-type)
  (next (:pointer :void))
  (is-active bool-32))

(defcstruct action-state-get-info
  ;; values = :TYPE-ACTION-STATE-GET-INFO
  (type structure-type)
  (next (:pointer :void))
  (action action)
  (subaction-path path) ;; optional
)

(defcstruct haptic-action-info
  ;; values = :TYPE-HAPTIC-ACTION-INFO
  (type structure-type)
  (next (:pointer :void))
  (action action)
  (subaction-path path) ;; optional
)

(defcstruct action-set-create-info
  ;; values = :TYPE-ACTION-SET-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  ;; length = +MAX-ACTION-SET-NAME-SIZE+
  (action-set-name :char :count 64)
  ;; length = +MAX-LOCALIZED-ACTION-SET-NAME-SIZE+
  (localized-action-set-name :char :count 128)
  (priority :uint32))

(defcstruct action-suggested-binding
  (action action)
  (binding path))

(defcstruct interaction-profile-suggested-binding
  ;; values = :TYPE-INTERACTION-PROFILE-SUGGESTED-BINDING
  (type structure-type)
  (next (:pointer :void))
  (interaction-profile path)
  (count-suggested-bindings :uint32)
  ;; length = count-suggested-bindings
  (suggested-bindings (:pointer (:struct action-suggested-binding))))

(defcstruct active-action-set
  (action-set action-set)
  (subaction-path path))

(defcstruct session-action-sets-attach-info
  ;; values = :TYPE-SESSION-ACTION-SETS-ATTACH-INFO
  (type structure-type)
  (next (:pointer :void))
  (count-action-sets :uint32)
  ;; length = count-action-sets
  (action-sets (:pointer action-set)))

(defcstruct actions-sync-info
  ;; values = :TYPE-ACTIONS-SYNC-INFO
  (type structure-type)
  (next (:pointer :void))
  (count-active-action-sets :uint32) ;; optional
  ;; length = count-active-action-sets
  (active-action-sets (:pointer (:struct active-action-set))) ;; optional
)

(defcstruct bound-sources-for-action-enumerate-info
  ;; values = :TYPE-BOUND-SOURCES-FOR-ACTION-ENUMERATE-INFO
  (type structure-type)
  (next (:pointer :void))
  (action action))

(defcstruct input-source-localized-name-get-info
  ;; values = :TYPE-INPUT-SOURCE-LOCALIZED-NAME-GET-INFO
  (type structure-type)
  (next (:pointer :void))
  (source-path path)
  (which-components input-source-localized-name-flags))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-interaction-profile-changed
  ;; values = :TYPE-EVENT-DATA-INTERACTION-PROFILE-CHANGED
  (type structure-type)
  (next (:pointer :void))
  (session session))

(defcstruct interaction-profile-state
  ;; values = :TYPE-INTERACTION-PROFILE-STATE
  (type structure-type)
  (next (:pointer :void))
  (interaction-profile path))

(defcstruct action-create-info
  ;; values = :TYPE-ACTION-CREATE-INFO
  (type structure-type)
  (next (:pointer :void))
  ;; length = +MAX-ACTION-NAME-SIZE+
  (action-name :char :count 64)
  (action-type action-type)
  (count-subaction-paths :uint32) ;; optional
  ;; length = count-subaction-paths
  (subaction-paths (:pointer path)) ;; optional
  ;; length = +MAX-LOCALIZED-ACTION-NAME-SIZE+
  (localized-action-name :char :count 128))

;;  extends : INSTANCE-CREATE-INFO
(defcstruct instance-create-info-android-khr
  ;; values = :TYPE-INSTANCE-CREATE-INFO-ANDROID-KHR
  (type structure-type)
  (next (:pointer :void))
  (application-vm (:pointer :void))
  (application-activity (:pointer :void)))

(defcstruct vulkan-swapchain-format-list-create-info-khr
  ;; values = :TYPE-VULKAN-SWAPCHAIN-FORMAT-LIST-CREATE-INFO-KHR
  (type structure-type)
  (next (:pointer :void))
  (view-format-count :uint32) ;; optional
  ;; length = view-format-count
  (view-formats (:pointer vk-format)))

(defcstruct debug-utils-object-name-info-ext
  ;; values = :TYPE-DEBUG-UTILS-OBJECT-NAME-INFO-EXT
  (type structure-type)
  (next (:pointer :void))
  (object-type object-type)
  (object-handle :uint64)
  ;; length = :null-terminated
  (object-name :string) ;; optional
)

(defcstruct debug-utils-label-ext
  ;; values = :TYPE-DEBUG-UTILS-LABEL-EXT
  (type structure-type)
  (next (:pointer :void))
  ;; length = :null-terminated
  (label-name :string))

(defcstruct debug-utils-messenger-callback-data-ext
  ;; values = :TYPE-DEBUG-UTILS-MESSENGER-CALLBACK-DATA-EXT
  (type structure-type)
  (next (:pointer :void))
  ;; length = :null-terminated
  (message-id :string)
  ;; length = :null-terminated
  (function-name :string)
  ;; length = :null-terminated
  (message :string)
  (object-count :uint32) ;; optional
  ;; length = object-count
  (objects (:pointer (:struct debug-utils-object-name-info-ext))) ;; optional, noautovalidity
  (session-label-count :uint32) ;; optional
  ;; length = session-label-count
  (session-labels (:pointer (:struct debug-utils-label-ext))) ;; optional, noautovalidity
)

;;  extends : INSTANCE-CREATE-INFO
(defcstruct debug-utils-messenger-create-info-ext
  ;; values = :TYPE-DEBUG-UTILS-MESSENGER-CREATE-INFO-EXT
  (type structure-type)
  (next (:pointer :void))
  (message-severities debug-utils-message-severity-flags-ext)
  (message-types debug-utils-message-type-flags-ext)
  (user-callback pfn-debug-utils-messenger-callback-ext)
  (user-data (:pointer :void)) ;; optional
)

;; struct types for XR_KHR_visibility_mask
(defcstruct visibility-mask-khr
  ;; values = :TYPE-VISIBILITY-MASK-KHR
  (type structure-type)
  (next (:pointer :void))
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32) ;; optional
  ;; length = vertex-capacity-input
  (vertices (:pointer (:struct vector-2f))) ;; optional
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32) ;; optional
  ;; length = index-capacity-input
  (indices (:pointer :uint32)) ;; optional
)

(defcstruct graphics-requirements-opengl-khr
  ;; values = :TYPE-GRAPHICS-REQUIREMENTS-OPENGL-KHR
  (type structure-type)
  (next (:pointer :void))
  (min-api-version-supported version)
  (max-api-version-supported version))

(defcstruct graphics-requirements-opengl-es-khr
  ;; values = :TYPE-GRAPHICS-REQUIREMENTS-OPENGL-ES-KHR
  (type structure-type)
  (next (:pointer :void))
  (min-api-version-supported version)
  (max-api-version-supported version))

(defcstruct graphics-requirements-vulkan-khr
  ;; values = :TYPE-GRAPHICS-REQUIREMENTS-VULKAN-KHR
  (type structure-type)
  (next (:pointer :void))
  (min-api-version-supported version)
  (max-api-version-supported version))

(defcstruct graphics-requirements-d3d11-khr
  ;; values = :TYPE-GRAPHICS-REQUIREMENTS-D3D11-KHR
  (type structure-type)
  (next (:pointer :void))
  (adapter-luid luid)
  (min-feature-level d3d-feature-level))

(defcstruct graphics-requirements-d3d12-khr
  ;; values = :TYPE-GRAPHICS-REQUIREMENTS-D3D12-KHR
  (type structure-type)
  (next (:pointer :void))
  (adapter-luid luid)
  (min-feature-level d3d-feature-level))

;; XR_KHR_vulkan_enable2 structs
(defcstruct vulkan-instance-create-info-khr
  ;; values = :TYPE-VULKAN-INSTANCE-CREATE-INFO-KHR
  (type structure-type)
  (next (:pointer :void))
  (system-id system-id)
  (create-flags vulkan-instance-create-flags-khr) ;; optional
  (pfn-get-instance-proc-addr vk-get-device-instance-proc-addr)
  (vulkan-create-info (:pointer vk-instance-create-info))
  (vulkan-allocator (:pointer vk-allocation-callbacks)) ;; optional
)

(defcstruct vulkan-device-create-info-khr
  ;; values = :TYPE-VULKAN-DEVICE-CREATE-INFO-KHR
  (type structure-type)
  (next (:pointer :void))
  (system-id system-id)
  (create-flags vulkan-device-create-flags-khr) ;; optional
  (pfn-get-instance-proc-addr vk-get-device-instance-proc-addr)
  (vulkan-physical-device vk-physical-device)
  (vulkan-create-info (:pointer vk-device-create-info))
  (vulkan-allocator (:pointer vk-allocation-callbacks)) ;; optional
)

;; alias struct GRAPHICS-BINDING-VULKAN-2-KHR
;;           -> GRAPHICS-BINDING-VULKAN-KHR
;;  extends : SESSION-CREATE-INFO
(defcstruct graphics-binding-vulkan-2-khr
  ;; values = :TYPE-GRAPHICS-BINDING-VULKAN-KHR
  (type structure-type)
  (next (:pointer :void))
  (instance vk-image)
  (physical-device vk-physical-device)
  (device vk-device)
  (queue-family-index :uint32)
  (queue-index :uint32))

(defcstruct vulkan-graphics-device-get-info-khr
  ;; values = :TYPE-VULKAN-GRAPHICS-DEVICE-GET-INFO-KHR
  (type structure-type)
  (next (:pointer :void))
  (system-id system-id)
  (vulkan-instance vk-image))

;; alias struct SWAPCHAIN-IMAGE-VULKAN-2-KHR
;;           -> SWAPCHAIN-IMAGE-VULKAN-KHR
;;  parent : SWAPCHAIN-IMAGE-BASE-HEADER
;;  returned only : true
(defcstruct swapchain-image-vulkan-2-khr
  ;; values = :TYPE-SWAPCHAIN-IMAGE-VULKAN-KHR
  (type structure-type)
  (next (:pointer :void))
  (image vk-image))

;; alias struct GRAPHICS-REQUIREMENTS-VULKAN-2-KHR
;;           -> GRAPHICS-REQUIREMENTS-VULKAN-KHR
(defcstruct graphics-requirements-vulkan-2-khr
  ;; values = :TYPE-GRAPHICS-REQUIREMENTS-VULKAN-KHR
  (type structure-type)
  (next (:pointer :void))
  (min-api-version-supported version)
  (max-api-version-supported version))

;; XR_META_vulkan_swapchain_create_info structs
;;  extends : SWAPCHAIN-CREATE-INFO
(defcstruct vulkan-swapchain-create-info-meta
  ;; values = :TYPE-VULKAN-SWAPCHAIN-CREATE-INFO-META
  (type structure-type)
  (next (:pointer :void))
  (additional-create-flags vk-image-create-flags)
  (additional-usage-flags vk-image-usage-flags))

;; XR_EXTX_overlay structs
;;  extends : SESSION-CREATE-INFO
(defcstruct session-create-info-overlay-extx
  ;; values = :TYPE-SESSION-CREATE-INFO-OVERLAY-EXTX
  (type structure-type)
  (next (:pointer :void))
  (create-flags overlay-session-create-flags-extx)
  (session-layers-placement :uint32))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-main-session-visibility-changed-extx
  ;; values = :TYPE-EVENT-DATA-MAIN-SESSION-VISIBILITY-CHANGED-EXTX
  (type structure-type)
  (next (:pointer :void))
  (visible bool-32)
  (flags overlay-main-session-flags-extx))

;; XR_FB_display_refresh_rate structs
;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-display-refresh-rate-changed-fb
  ;; values = :TYPE-EVENT-DATA-DISPLAY-REFRESH-RATE-CHANGED-FB
  (type structure-type)
  (next (:pointer :void))
  (from-display-refresh-rate :float)
  (to-display-refresh-rate :float))

;; struct types for XR_EXT_view_configuration_depth_range
;;  extends : VIEW-CONFIGURATION-VIEW
(defcstruct view-configuration-depth-range-ext
  ;; values = :TYPE-VIEW-CONFIGURATION-DEPTH-RANGE-EXT
  (type structure-type)
  (next (:pointer :void))
  (recommended-near-z :float)
  (min-near-z :float)
  (recommended-far-z :float)
  (max-far-z :float))

;; struct types for XR_EPIC_view_configuration_fov
;;  extends : VIEW-CONFIGURATION-VIEW
(defcstruct view-configuration-view-fov-epic
  ;; values = :TYPE-VIEW-CONFIGURATION-VIEW-FOV-EPIC
  (type structure-type)
  (next (:pointer :void))
  (recommended-fov (:struct fov-f))
  (max-mutable-fov (:struct fov-f)))

;; struct types for XR_EXT_dpad_binding
;;  parent : BINDING-MODIFICATION-BASE-HEADER-KHR
(defcstruct interaction-profile-dpad-binding-ext
  ;; values = :TYPE-INTERACTION-PROFILE-DPAD-BINDING-EXT
  (type structure-type)
  (next (:pointer :void))
  (binding path)
  (action-set action-set)
  (force-threshold :float)
  (force-threshold-released :float)
  (center-region :float)
  (wedge-angle :float)
  (is-sticky bool-32)
  (on-haptic (:pointer (:struct haptic-base-header))) ;; optional
  (off-haptic (:pointer (:struct haptic-base-header))) ;; optional
)

;; struct types for XR_VALVE_analog_threshold
;;  parent : BINDING-MODIFICATION-BASE-HEADER-KHR
(defcstruct interaction-profile-analog-threshold-valve
  ;; values = :TYPE-INTERACTION-PROFILE-ANALOG-THRESHOLD-VALVE
  (type structure-type)
  (next (:pointer :void))
  (action action)
  (binding path)
  (on-threshold :float)
  (off-threshold :float)
  (on-haptic (:pointer (:struct haptic-base-header))) ;; optional
  (off-haptic (:pointer (:struct haptic-base-header))) ;; optional
)

(defcstruct binding-modification-base-header-khr
  (type structure-type) ;; noautovalidity
  (next (:pointer :void)))

;; struct types for XR_KHR_binding_modification
;;  extends : INTERACTION-PROFILE-SUGGESTED-BINDING
(defcstruct binding-modifications-khr
  ;; values = :TYPE-BINDING-MODIFICATIONS-KHR
  (type structure-type)
  (next (:pointer :void))
  (binding-modification-count :uint32) ;; optional
  ;; length = binding-modification-count
  (binding-modifications (:pointer
                          (:pointer
                           (:struct binding-modification-base-header-khr)))) ;; optional
)

;; types for XR_EXT_eye_gaze_interaction
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-eye-gaze-interaction-properties-ext
  ;; values = :TYPE-SYSTEM-EYE-GAZE-INTERACTION-PROPERTIES-EXT
  (type structure-type)
  (next (:pointer :void))
  (supports-eye-gaze-interaction bool-32))

;;  extends : SPACE-LOCATION
(defcstruct eye-gaze-sample-time-ext
  ;; values = :TYPE-EYE-GAZE-SAMPLE-TIME-EXT
  (type structure-type)
  (next (:pointer :void))
  (time time))

;; types for XR_MSFT_spatial_anchor
(defcstruct spatial-anchor-create-info-msft
  ;; values = :TYPE-SPATIAL-ANCHOR-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (space space)
  (pose (:struct pose-f))
  (time time))

(defcstruct spatial-anchor-space-create-info-msft
  ;; values = :TYPE-SPATIAL-ANCHOR-SPACE-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (anchor spatial-anchor-msft)
  (pose-in-anchor-space (:struct pose-f)))

;; types for XR_FB_composition_layer_image_layout
;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-image-layout-fb
  ;; values = :TYPE-COMPOSITION-LAYER-IMAGE-LAYOUT-FB
  (type structure-type)
  (next (:pointer :void))
  (flags composition-layer-image-layout-flags-fb) ;; optional
)

;; types for XR_FB_composition_layer_alpha_blend
;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-alpha-blend-fb
  ;; values = :TYPE-COMPOSITION-LAYER-ALPHA-BLEND-FB
  (type structure-type)
  (next (:pointer :void))
  (src-factor-color blend-factor-fb)
  (dst-factor-color blend-factor-fb)
  (src-factor-alpha blend-factor-fb)
  (dst-factor-alpha blend-factor-fb))

;; types for XR_MNDX_egl_enable
;;  extends : SESSION-CREATE-INFO
(defcstruct graphics-binding-eglmndx
  ;; values = :TYPE-GRAPHICS-BINDING-EGL-MNDX
  (type structure-type)
  (next (:pointer :void))
  (get-proc-address pfn-egl-get-proc-address-proc)
  (display egl-display)
  (config egl-config)
  (context egl-context))

(defcstruct spatial-graph-node-space-create-info-msft
  ;; values = :TYPE-SPATIAL-GRAPH-NODE-SPACE-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (node-type spatial-graph-node-type-msft)
  ;; length = +GUID-SIZE-MSFT+
  (node-id :uint8 :count 16)
  (pose (:struct pose-f)))

(defcstruct spatial-graph-static-node-binding-create-info-msft
  ;; values = :TYPE-SPATIAL-GRAPH-STATIC-NODE-BINDING-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (space space)
  (pose-in-space (:struct pose-f))
  (time time))

(defcstruct spatial-graph-node-binding-properties-get-info-msft
  ;; values = :TYPE-SPATIAL-GRAPH-NODE-BINDING-PROPERTIES-GET-INFO-MSFT
  (type structure-type)
  (next (:pointer :void)))

(defcstruct spatial-graph-node-binding-properties-msft
  ;; values = :TYPE-SPATIAL-GRAPH-NODE-BINDING-PROPERTIES-MSFT
  (type structure-type)
  (next (:pointer :void))
  ;; length = +GUID-SIZE-MSFT+
  (node-id :uint8 :count 16)
  (pose-in-node-space (:struct pose-f)))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-hand-tracking-properties-ext
  ;; values = :TYPE-SYSTEM-HAND-TRACKING-PROPERTIES-EXT
  (type structure-type)
  (next (:pointer :void))
  (supports-hand-tracking bool-32))

(defcstruct hand-tracker-create-info-ext
  ;; values = :TYPE-HAND-TRACKER-CREATE-INFO-EXT
  (type structure-type)
  (next (:pointer :void))
  (hand hand-ext)
  (hand-joint-set hand-joint-set-ext))

(defcstruct hand-joints-locate-info-ext
  ;; values = :TYPE-HAND-JOINTS-LOCATE-INFO-EXT
  (type structure-type)
  (next (:pointer :void))
  (base-space space)
  (time time))

(defcstruct hand-joint-location-ext
  (location-flags space-location-flags) ;; optional
  (pose (:struct pose-f))
  (radius :float))

(defcstruct hand-joint-velocity-ext
  (velocity-flags space-velocity-flags)
  (linear-velocity (:struct vector-3f))
  (angular-velocity (:struct vector-3f)))

(defcstruct hand-joint-locations-ext
  ;; values = :TYPE-HAND-JOINT-LOCATIONS-EXT
  (type structure-type)
  (next (:pointer :void))
  (is-active bool-32)
  (joint-count :uint32)
  ;; length = joint-count
  (joint-locations (:pointer (:struct hand-joint-location-ext))))

;;  extends : HAND-JOINT-LOCATIONS-EXT
(defcstruct hand-joint-velocities-ext
  ;; values = :TYPE-HAND-JOINT-VELOCITIES-EXT
  (type structure-type)
  (next (:pointer :void))
  (joint-count :uint32)
  ;; length = joint-count
  (joint-velocities (:pointer (:struct hand-joint-velocity-ext))))

;; XR_FB_face_tracking structs
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-face-tracking-properties-fb
  ;; values = :TYPE-SYSTEM-FACE-TRACKING-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-face-tracking bool-32))

(defcstruct face-tracker-create-info-fb
  ;; values = :TYPE-FACE-TRACKER-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (face-expression-set face-expression-set-fb))

(defcstruct face-expression-info-fb
  ;; values = :TYPE-FACE-EXPRESSION-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (time time))

(defcstruct face-expression-status-fb
  (is-valid bool-32)
  (is-eye-following-blendshapes-valid bool-32))

(defcstruct face-expression-weights-fb
  ;; values = :TYPE-FACE-EXPRESSION-WEIGHTS-FB
  (type structure-type)
  (next (:pointer :void))
  (weight-count :uint32)
  ;; length = weight-count
  (weights (:pointer :float))
  (confidence-count :uint32)
  ;; length = confidence-count
  (confidences (:pointer :float))
  (status (:struct face-expression-status-fb))
  (time time))

;; XR_FB_body_tracking structs
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-body-tracking-properties-fb
  ;; values = :TYPE-SYSTEM-BODY-TRACKING-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-body-tracking bool-32))

(defcstruct body-tracker-create-info-fb
  ;; values = :TYPE-BODY-TRACKER-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (body-joint-set body-joint-set-fb))

(defcstruct body-skeleton-joint-fb
  (joint :int32)
  (parent-joint :int32)
  (pose (:struct pose-f)))

(defcstruct body-skeleton-fb
  ;; values = :TYPE-BODY-SKELETON-FB
  (type structure-type)
  (next (:pointer :void))
  (joint-count :uint32)
  ;; length = joint-count
  (joints (:pointer (:struct body-skeleton-joint-fb))))

(defcstruct body-joints-locate-info-fb
  ;; values = :TYPE-BODY-JOINTS-LOCATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (base-space space)
  (time time))

(defcstruct body-joint-location-fb
  (location-flags space-location-flags)
  (pose (:struct pose-f)))

(defcstruct body-joint-locations-fb
  ;; values = :TYPE-BODY-JOINT-LOCATIONS-FB
  (type structure-type)
  (next (:pointer :void))
  (is-active bool-32)
  (confidence :float)
  (joint-count :uint32)
  ;; length = joint-count
  (joint-locations (:pointer (:struct body-joint-location-fb)))
  (skeleton-changed-count :uint32)
  (time time))

;; XR_FB_eye_tracking_social structs
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-eye-tracking-properties-fb
  ;; values = :TYPE-SYSTEM-EYE-TRACKING-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-eye-tracking bool-32))

(defcstruct eye-tracker-create-info-fb
  ;; values = :TYPE-EYE-TRACKER-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void)))

(defcstruct eye-gazes-info-fb
  ;; values = :TYPE-EYE-GAZES-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (base-space space)
  (time time))

(defcstruct eye-gaze-fb
  (is-valid bool-32)
  (gaze-pose (:struct pose-f))
  (gaze-confidence :float))

(defcstruct eye-gazes-fb
  ;; values = :TYPE-EYE-GAZES-FB
  (type structure-type)
  (next (:pointer :void))
  ;; length = +EYE-POSITION-COUNT-FB+
  (gaze (:struct eye-gaze-fb) :count 2)
  (time time))

;;  extends : HAND-JOINTS-LOCATE-INFO-EXT
(defcstruct hand-joints-motion-range-info-ext
  ;; values = :TYPE-HAND-JOINTS-MOTION-RANGE-INFO-EXT
  (type structure-type)
  (next (:pointer :void))
  (hand-joints-motion-range hand-joints-motion-range-ext))

(defcstruct hand-mesh-space-create-info-msft
  ;; values = :TYPE-HAND-MESH-SPACE-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (hand-pose-type hand-pose-type-msft)
  (pose-in-hand-mesh-space (:struct pose-f)))

(defcstruct hand-mesh-update-info-msft
  ;; values = :TYPE-HAND-MESH-UPDATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (time time)
  (hand-pose-type hand-pose-type-msft))

(defcstruct hand-mesh-vertex-msft
  (position (:struct vector-3f))
  (normal (:struct vector-3f)))

(defcstruct hand-mesh-vertex-buffer-msft
  (vertex-update-time time) ;; optional
  (vertex-capacity-input :uint32)
  (vertex-count-output :uint32) ;; optional
  ;; length = vertex-capacity-input
  (vertices (:pointer (:struct hand-mesh-vertex-msft))))

(defcstruct hand-mesh-index-buffer-msft
  (index-buffer-key :uint32) ;; optional
  (index-capacity-input :uint32)
  (index-count-output :uint32) ;; optional
  ;; length = index-capacity-input
  (indices (:pointer :uint32)))

(defcstruct hand-mesh-msft
  ;; values = :TYPE-HAND-MESH-MSFT
  (type structure-type)
  (next (:pointer :void))
  (is-active bool-32)
  (index-buffer-changed bool-32)
  (vertex-buffer-changed bool-32)
  (index-buffer (:struct hand-mesh-index-buffer-msft))
  (vertex-buffer (:struct hand-mesh-vertex-buffer-msft)))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-hand-tracking-mesh-properties-msft
  ;; values = :TYPE-SYSTEM-HAND-TRACKING-MESH-PROPERTIES-MSFT
  (type structure-type)
  (next (:pointer :void))
  (supports-hand-tracking-mesh bool-32)
  (max-hand-mesh-index-count :uint32)
  (max-hand-mesh-vertex-count :uint32))

;;  extends : HAND-TRACKER-CREATE-INFO-EXT
(defcstruct hand-pose-type-info-msft
  ;; values = :TYPE-HAND-POSE-TYPE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (hand-pose-type hand-pose-type-msft))

;; XR_MSFT_secondary_view_configuration
;;  extends : SESSION-BEGIN-INFO
(defcstruct secondary-view-configuration-session-begin-info-msft
  ;; values = :TYPE-SECONDARY-VIEW-CONFIGURATION-SESSION-BEGIN-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-count :uint32)
  ;; length = view-configuration-count
  (enabled-view-configuration-types (:pointer view-configuration-type)))

(defcstruct secondary-view-configuration-state-msft
  ;; values = :TYPE-SECONDARY-VIEW-CONFIGURATION-STATE-MSFT
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-type view-configuration-type)
  (active bool-32))

;;  extends : FRAME-STATE
(defcstruct secondary-view-configuration-frame-state-msft
  ;; values = :TYPE-SECONDARY-VIEW-CONFIGURATION-FRAME-STATE-MSFT
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-count :uint32)
  ;; length = view-configuration-count
  (view-configuration-states (:pointer
                              (:struct secondary-view-configuration-state-msft))))

(defcstruct secondary-view-configuration-layer-info-msft
  ;; values = :TYPE-SECONDARY-VIEW-CONFIGURATION-LAYER-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-type view-configuration-type)
  (environment-blend-mode environment-blend-mode)
  (layer-count :uint32)
  ;; length = layer-count
  (layers (:pointer (:pointer (:struct composition-layer-base-header)))))

;;  extends : FRAME-END-INFO
(defcstruct secondary-view-configuration-frame-end-info-msft
  ;; values = :TYPE-SECONDARY-VIEW-CONFIGURATION-FRAME-END-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-count :uint32)
  ;; length = view-configuration-count
  (view-configuration-layers-info (:pointer
                                   (:struct
                                    secondary-view-configuration-layer-info-msft))))

;;  extends : SWAPCHAIN-CREATE-INFO
(defcstruct secondary-view-configuration-swapchain-create-info-msft
  ;; values = :TYPE-SECONDARY-VIEW-CONFIGURATION-SWAPCHAIN-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (view-configuration-type view-configuration-type))

;; XR_MSFT_holographic_window_attachment
;;  extends : SESSION-CREATE-INFO
;;  protect : XR_USE_PLATFORM_WIN32
(defcstruct holographic-window-attachment-msft
  ;; values = :TYPE-HOLOGRAPHIC-WINDOW-ATTACHMENT-MSFT
  (type structure-type)
  (next (:pointer :void))
  (holographic-space (:pointer i-unknown))
  (core-window (:pointer i-unknown)))

;; XR_FB_android_surface_swapchain_create
;;  extends : SWAPCHAIN-CREATE-INFO
;;  protect : XR_USE_PLATFORM_ANDROID
(defcstruct android-surface-swapchain-create-info-fb
  ;; values = :TYPE-ANDROID-SURFACE-SWAPCHAIN-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (create-flags android-surface-swapchain-flags-fb))

;; XR_FB_swapchain_update_state structs
(defcstruct swapchain-state-base-header-fb
  (type structure-type)
  (next (:pointer :void)))

;; XR_FB_swapchain_update_state_android_surface structs
;;  parent : SWAPCHAIN-STATE-BASE-HEADER-FB
;;  protect : XR_USE_PLATFORM_ANDROID
(defcstruct swapchain-state-android-surface-dimensions-fb
  ;; values = :TYPE-SWAPCHAIN-STATE-ANDROID-SURFACE-DIMENSIONS-FB
  (type structure-type)
  (next (:pointer :void))
  (width :uint32)
  (height :uint32))

;; XR_FB_swapchain_update_state_opengl_es structs
;;  parent : SWAPCHAIN-STATE-BASE-HEADER-FB
;;  protect : XR_USE_GRAPHICS_API_OPENGL_ES
(defcstruct swapchain-state-sampler-opengl-es-fb
  ;; values = :TYPE-SWAPCHAIN-STATE-SAMPLER-OPENGL-ES-FB
  (type structure-type)
  (next (:pointer :void))
  (min-filter egl-enum)
  (mag-filter egl-enum)
  (wrap-mode-s egl-enum)
  (wrap-mode-t egl-enum)
  (swizzle-red egl-enum)
  (swizzle-green egl-enum)
  (swizzle-blue egl-enum)
  (swizzle-alpha egl-enum)
  (max-anisotropy :float)
  (border-color (:struct color-4f)))

;; XR_FB_swapchain_update_state_vulkan structs
;;  parent : SWAPCHAIN-STATE-BASE-HEADER-FB
;;  protect : XR_USE_GRAPHICS_API_VULKAN
(defcstruct swapchain-state-sampler-vulkan-fb
  ;; values = :TYPE-SWAPCHAIN-STATE-SAMPLER-VULKAN-FB
  (type structure-type)
  (next (:pointer :void))
  (min-filter vk-filter)
  (mag-filter vk-filter)
  (mipmap-mode vk-sampler-mipmap-mode)
  (wrap-mode-s vk-sampler-address-mode)
  (wrap-mode-t vk-sampler-address-mode)
  (swizzle-red vk-component-swizzle)
  (swizzle-green vk-component-swizzle)
  (swizzle-blue vk-component-swizzle)
  (swizzle-alpha vk-component-swizzle)
  (max-anisotropy :float)
  (border-color (:struct color-4f)))

;; XR_FB_composition_layer_secure_content structs
;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-secure-content-fb
  ;; values = :TYPE-COMPOSITION-LAYER-SECURE-CONTENT-FB
  (type structure-type)
  (next (:pointer :void))
  (flags composition-layer-secure-content-flags-fb))

;; XR_KHR_loader_init
(defcstruct loader-init-info-base-header-khr
  (type structure-type)
  (next (:pointer :void)))

;; XR_KHR_loader_init_android
;;  parent : LOADER-INIT-INFO-BASE-HEADER-KHR
(defcstruct loader-init-info-android-khr
  ;; values = :TYPE-LOADER-INIT-INFO-ANDROID-KHR
  (type structure-type)
  (next (:pointer :void))
  (application-vm (:pointer :void))
  (application-context (:pointer :void)))

;; XR_KHR_composition_layer_equirect2
;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-equirect-2-khr
  ;; values = :TYPE-COMPOSITION-LAYER-EQUIRECT2-KHR
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (radius :float)
  (central-horizontal-angle :float)
  (upper-vertical-angle :float)
  (lower-vertical-angle :float))

;; XR_KHR_composition_layer_color_scale_bias
;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-color-scale-bias-khr
  ;; values = :TYPE-COMPOSITION-LAYER-COLOR-SCALE-BIAS-KHR
  (type structure-type)
  (next (:pointer :void))
  (color-scale (:struct color-4f))
  (color-bias (:struct color-4f)))

;; XR_MSFT_controller_model
(defcstruct controller-model-key-state-msft
  ;; values = :TYPE-CONTROLLER-MODEL-KEY-STATE-MSFT
  (type structure-type)
  (next (:pointer :void))
  (model-key controller-model-key-msft))

(defcstruct controller-model-node-properties-msft
  ;; values = :TYPE-CONTROLLER-MODEL-NODE-PROPERTIES-MSFT
  (type structure-type)
  (next (:pointer :void))
  ;; length = +MAX-CONTROLLER-MODEL-NODE-NAME-SIZE-MSFT+
  (parent-node-name :char :count 64)
  ;; length = +MAX-CONTROLLER-MODEL-NODE-NAME-SIZE-MSFT+
  (node-name :char :count 64))

(defcstruct controller-model-properties-msft
  ;; values = :TYPE-CONTROLLER-MODEL-PROPERTIES-MSFT
  (type structure-type)
  (next (:pointer :void))
  (node-capacity-input :uint32) ;; optional
  (node-count-output :uint32) ;; optional
  ;; length = node-capacity-input
  (node-properties (:pointer (:struct controller-model-node-properties-msft))) ;; optional
)

(defcstruct controller-model-node-state-msft
  ;; values = :TYPE-CONTROLLER-MODEL-NODE-STATE-MSFT
  (type structure-type)
  (next (:pointer :void))
  (node-pose (:struct pose-f)))

(defcstruct controller-model-state-msft
  ;; values = :TYPE-CONTROLLER-MODEL-STATE-MSFT
  (type structure-type)
  (next (:pointer :void))
  (node-capacity-input :uint32) ;; optional
  (node-count-output :uint32) ;; optional
  ;; length = node-capacity-input
  (node-states (:pointer (:struct controller-model-node-state-msft))) ;; optional
)

(defcstruct uuid-msft
  (bytes :uint8 :count 16))

(defcstruct scene-observer-create-info-msft
  ;; values = :TYPE-SCENE-OBSERVER-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void)))

(defcstruct scene-create-info-msft
  ;; values = :TYPE-SCENE-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void)))

(defcstruct scene-frustum-bound-msft
  (pose (:struct pose-f))
  (fov (:struct fov-f))
  (far-distance :float))

(defcstruct scene-oriented-box-bound-msft
  (pose (:struct pose-f))
  (extents (:struct vector-3f)))

(defcstruct scene-sphere-bound-msft
  (center (:struct vector-3f))
  (radius :float))

(defcstruct scene-bounds-msft
  (space space)
  (time time)
  (sphere-count :uint32) ;; optional
  ;; length = sphere-count
  (spheres (:pointer (:struct scene-sphere-bound-msft))) ;; optional
  (box-count :uint32) ;; optional
  ;; length = box-count
  (boxes (:pointer (:struct scene-oriented-box-bound-msft))) ;; optional
  (frustum-count :uint32) ;; optional
  ;; length = frustum-count
  (frustums (:pointer (:struct scene-frustum-bound-msft))) ;; optional
)

(defcstruct new-scene-compute-info-msft
  ;; values = :TYPE-NEW-SCENE-COMPUTE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (requested-feature-count :uint32)
  ;; length = requested-feature-count
  (requested-features (:pointer scene-compute-feature-msft))
  (consistency scene-compute-consistency-msft)
  (bounds (:struct scene-bounds-msft)))

;;  extends : NEW-SCENE-COMPUTE-INFO-MSFT
(defcstruct visual-mesh-compute-lod-info-msft
  ;; values = :TYPE-VISUAL-MESH-COMPUTE-LOD-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (lod mesh-compute-lod-msft))

(defcstruct scene-component-msft
  (component-type scene-component-type-msft)
  (id (:struct uuid-msft))
  (parent-id (:struct uuid-msft)) ;; optional
  (update-time time))

(defcstruct scene-components-msft
  ;; values = :TYPE-SCENE-COMPONENTS-MSFT
  (type structure-type)
  (next (:pointer :void))
  (component-capacity-input :uint32) ;; optional
  (component-count-output :uint32)
  ;; length = component-capacity-input
  (components (:pointer (:struct scene-component-msft))) ;; optional
)

(defcstruct scene-components-get-info-msft
  ;; values = :TYPE-SCENE-COMPONENTS-GET-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (component-type scene-component-type-msft))

(defcstruct scene-component-location-msft
  (flags space-location-flags) ;; optional
  (pose (:struct pose-f)))

(defcstruct scene-component-locations-msft
  ;; values = :TYPE-SCENE-COMPONENT-LOCATIONS-MSFT
  (type structure-type)
  (next (:pointer :void))
  (location-count :uint32) ;; optional
  ;; length = location-count
  (locations (:pointer (:struct scene-component-location-msft))) ;; optional
)

(defcstruct scene-components-locate-info-msft
  ;; values = :TYPE-SCENE-COMPONENTS-LOCATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (base-space space)
  (time time)
  (component-id-count :uint32) ;; optional
  ;; length = component-id-count
  (component-ids (:pointer (:struct uuid-msft))) ;; optional
)

(defcstruct scene-object-msft
  (object-type scene-object-type-msft))

;;  extends : SCENE-COMPONENTS-MSFT
(defcstruct scene-objects-msft
  ;; values = :TYPE-SCENE-OBJECTS-MSFT
  (type structure-type)
  (next (:pointer :void))
  (scene-object-count :uint32) ;; optional
  ;; length = scene-object-count
  (scene-objects (:pointer (:struct scene-object-msft))) ;; optional
)

;;  extends : SCENE-COMPONENTS-GET-INFO-MSFT
(defcstruct scene-component-parent-filter-info-msft
  ;; values = :TYPE-SCENE-COMPONENT-PARENT-FILTER-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (parent-id (:struct uuid-msft)))

;;  extends : SCENE-COMPONENTS-GET-INFO-MSFT
(defcstruct scene-object-types-filter-info-msft
  ;; values = :TYPE-SCENE-OBJECT-TYPES-FILTER-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (object-type-count :uint32) ;; optional
  ;; length = object-type-count
  (object-types (:pointer scene-object-type-msft)) ;; optional
)

(defcstruct scene-plane-msft
  (alignment scene-plane-alignment-type-msft)
  (size (:struct extent-2d-f))
  (mesh-buffer-id :uint64)
  (supports-indices-uint-16 bool-32))

;;  extends : SCENE-COMPONENTS-MSFT
(defcstruct scene-planes-msft
  ;; values = :TYPE-SCENE-PLANES-MSFT
  (type structure-type)
  (next (:pointer :void))
  (scene-plane-count :uint32) ;; optional
  ;; length = scene-plane-count
  (scene-planes (:pointer (:struct scene-plane-msft))) ;; optional
)

;;  extends : SCENE-COMPONENTS-GET-INFO-MSFT
(defcstruct scene-plane-alignment-filter-info-msft
  ;; values = :TYPE-SCENE-PLANE-ALIGNMENT-FILTER-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (alignment-count :uint32) ;; optional
  ;; length = alignment-count
  (alignments (:pointer scene-plane-alignment-type-msft)) ;; optional
)

(defcstruct scene-mesh-msft
  (mesh-buffer-id :uint64)
  (supports-indices-uint-16 bool-32))

;;  extends : SCENE-COMPONENTS-MSFT
(defcstruct scene-meshes-msft
  ;; values = :TYPE-SCENE-MESHES-MSFT
  (type structure-type)
  (next (:pointer :void))
  (scene-mesh-count :uint32) ;; optional
  ;; length = scene-mesh-count
  (scene-meshes (:pointer (:struct scene-mesh-msft))) ;; optional
)

(defcstruct scene-mesh-buffers-get-info-msft
  ;; values = :TYPE-SCENE-MESH-BUFFERS-GET-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (mesh-buffer-id :uint64))

(defcstruct scene-mesh-buffers-msft
  ;; values = :TYPE-SCENE-MESH-BUFFERS-MSFT
  (type structure-type)
  (next (:pointer :void)))

(defcstruct scene-mesh-vertex-buffer-msft
  ;; values = :TYPE-SCENE-MESH-VERTEX-BUFFER-MSFT
  (type structure-type)
  (next (:pointer :void))
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32)
  ;; length = vertex-capacity-input
  (vertices (:pointer (:struct vector-3f))) ;; optional
)

(defcstruct scene-mesh-indices-uint-32-msft
  ;; values = :TYPE-SCENE-MESH-INDICES-UINT32-MSFT
  (type structure-type)
  (next (:pointer :void))
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32)
  ;; length = index-capacity-input
  (indices (:pointer :uint32)) ;; optional
)

(defcstruct scene-mesh-indices-uint-16-msft
  ;; values = :TYPE-SCENE-MESH-INDICES-UINT16-MSFT
  (type structure-type)
  (next (:pointer :void))
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32)
  ;; length = index-capacity-input
  (indices (:pointer :uint16)) ;; optional
)

;; XR_MSFT_scene_understanding_serialization
(defcstruct serialized-scene-fragment-data-get-info-msft
  ;; values = :TYPE-SERIALIZED-SCENE-FRAGMENT-DATA-GET-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (scene-fragment-id (:struct uuid-msft)))

(defcstruct deserialize-scene-fragment-msft
  (buffer-size :uint32) ;; optional
  ;; length = buffer-size
  (buffer (:pointer :uint8)) ;; optional
)

(defcstruct scene-deserialize-info-msft
  ;; values = :TYPE-SCENE-DESERIALIZE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (fragment-count :uint32) ;; optional
  ;; length = fragment-count
  (fragments (:pointer (:struct deserialize-scene-fragment-msft))) ;; optional
)

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-color-space-properties-fb
  ;; values = :TYPE-SYSTEM-COLOR-SPACE-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (color-space color-space-fb))

;; XR_FB_spatial_entity structs
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-spatial-entity-properties-fb
  ;; values = :TYPE-SYSTEM-SPATIAL-ENTITY-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-spatial-entity bool-32))

(defcstruct spatial-anchor-create-info-fb
  ;; values = :TYPE-SPATIAL-ANCHOR-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (space space)
  (pose-in-space (:struct pose-f))
  (time time))

(defcstruct space-component-status-set-info-fb
  ;; values = :TYPE-SPACE-COMPONENT-STATUS-SET-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (component-type space-component-type-fb)
  (enabled bool-32)
  (timeout duration))

;;  returned only : true
(defcstruct space-component-status-fb
  ;; values = :TYPE-SPACE-COMPONENT-STATUS-FB
  (type structure-type)
  (next (:pointer :void))
  (enabled bool-32)
  (change-pending bool-32))

;; XR_EXT_uuid
(defcstruct uuid-ext
  ;; length = +UUID-SIZE-EXT+
  (data :uint8 :count 16))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-spatial-anchor-create-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPATIAL-ANCHOR-CREATE-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext)))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-set-status-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-SET-STATUS-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext))
  (component-type space-component-type-fb)
  (enabled bool-32))

;; XR_FB_foveation structs
(defcstruct foveation-profile-create-info-fb
  ;; values = :TYPE-FOVEATION-PROFILE-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void)))

;;  extends : SWAPCHAIN-CREATE-INFO
(defcstruct swapchain-create-info-foveation-fb
  ;; values = :TYPE-SWAPCHAIN-CREATE-INFO-FOVEATION-FB
  (type structure-type)
  (next (:pointer :void))
  (flags swapchain-create-foveation-flags-fb) ;; optional
)

;;  parent : SWAPCHAIN-STATE-BASE-HEADER-FB
(defcstruct swapchain-state-foveation-fb
  ;; values = :TYPE-SWAPCHAIN-STATE-FOVEATION-FB
  (type structure-type)
  (next (:pointer :void))
  (flags swapchain-state-foveation-flags-fb) ;; optional
  (profile foveation-profile-fb))

;; XR_FB_foveation_vulkan structs
;;  extends : SWAPCHAIN-IMAGE-VULKAN-KHR
;;  returned only : true
(defcstruct swapchain-image-foveation-vulkan-fb
  ;; values = :TYPE-SWAPCHAIN-IMAGE-FOVEATION-VULKAN-FB
  (type structure-type)
  (next (:pointer :void))
  (image vk-image)
  (width :uint32)
  (height :uint32))

;;  extends : FOVEATION-PROFILE-CREATE-INFO-FB
(defcstruct foveation-level-profile-create-info-fb
  ;; values = :TYPE-FOVEATION-LEVEL-PROFILE-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (level foveation-level-fb)
  (vertical-offset :float)
  (dynamic foveation-dynamic-fb))

;; XR_META_foveation_eye_tracked structs
;;  extends : FOVEATION-LEVEL-PROFILE-CREATE-INFO-FB
(defcstruct foveation-eye-tracked-profile-create-info-meta
  ;; values = :TYPE-FOVEATION-EYE-TRACKED-PROFILE-CREATE-INFO-META
  (type structure-type)
  (next (:pointer :void))
  (flags foveation-eye-tracked-profile-create-flags-meta))

;;  returned only : true
(defcstruct foveation-eye-tracked-state-meta
  ;; values = :TYPE-FOVEATION-EYE-TRACKED-STATE-META
  (type structure-type)
  (next (:pointer :void))
  ;; length = +FOVEATION-CENTER-SIZE-META+
  (foveation-center (:struct vector-2f) :count 2)
  (flags foveation-eye-tracked-state-flags-meta))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-foveation-eye-tracked-properties-meta
  ;; values = :TYPE-SYSTEM-FOVEATION-EYE-TRACKED-PROPERTIES-META
  (type structure-type)
  (next (:pointer :void))
  (supports-foveation-eye-tracked bool-32))

;; XR_FB_hand_tracking_mesh structs
(defcstruct vector-4s-fb
  (x :int16)
  (y :int16)
  (z :int16)
  (w :int16))

(defcstruct hand-tracking-mesh-fb
  ;; values = :TYPE-HAND-TRACKING-MESH-FB
  (type structure-type)
  (next (:pointer :void))
  (joint-capacity-input :uint32) ;; optional
  (joint-count-output :uint32) ;; optional
  ;; length = joint-capacity-input
  (joint-bind-poses (:pointer (:struct pose-f))) ;; optional
  ;; length = joint-capacity-input
  (joint-radi-i (:pointer :float)) ;; optional
  ;; length = joint-capacity-input
  (joint-parents (:pointer hand-joint-ext)) ;; optional
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32) ;; optional
  ;; length = vertex-capacity-input
  (vertex-positions (:pointer (:struct vector-3f))) ;; optional
  ;; length = vertex-capacity-input
  (vertex-normals (:pointer (:struct vector-3f))) ;; optional
  ;; length = vertex-capacity-input
  (vertex-uvs (:pointer (:struct vector-2f))) ;; optional
  ;; length = vertex-capacity-input
  (vertex-blend-indices (:pointer (:struct vector-4s-fb))) ;; optional
  ;; length = vertex-capacity-input
  (vertex-blend-weights (:pointer (:struct vector-4f))) ;; optional
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32) ;; optional
  ;; length = index-capacity-input
  (indices (:pointer :int16)) ;; optional
)

;;  extends : HAND-JOINT-LOCATIONS-EXT
;;  returned only : true
(defcstruct hand-tracking-scale-fb
  ;; values = :TYPE-HAND-TRACKING-SCALE-FB
  (type structure-type)
  (next (:pointer :void))
  (sensor-output :float)
  (current-output :float)
  (override-hand-scale bool-32)
  (override-value-input :float) ;; optional
)

;; XR_FB_hand_tracking_aim structs
;;  extends : HAND-JOINT-LOCATIONS-EXT
;;  returned only : true
(defcstruct hand-tracking-aim-state-fb
  ;; values = :TYPE-HAND-TRACKING-AIM-STATE-FB
  (type structure-type)
  (next (:pointer :void))
  (status hand-tracking-aim-flags-fb)
  (aim-pose (:struct pose-f))
  (pinch-strength-index :float)
  (pinch-strength-middle :float)
  (pinch-strength-ring :float)
  (pinch-strength-little :float))

;; XR_FB_hand_tracking_capsules structs
;;  returned only : true
(defcstruct hand-capsule-fb
  ;; length = +HAND-TRACKING-CAPSULE-POINT-COUNT-FB+
  (points (:struct vector-3f) :count 2)
  (radius :float)
  (joint hand-joint-ext))

;;  extends : HAND-JOINT-LOCATIONS-EXT
;;  returned only : true
(defcstruct hand-tracking-capsules-state-fb
  ;; values = :TYPE-HAND-TRACKING-CAPSULES-STATE-FB
  (type structure-type)
  (next (:pointer :void))
  ;; length = +HAND-TRACKING-CAPSULE-COUNT-FB+
  (capsules (:struct hand-capsule-fb) :count 19))

;; XR_FB_render_model structs
(defcstruct render-model-path-info-fb
  ;; values = :TYPE-RENDER-MODEL-PATH-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (path path))

(defcstruct render-model-properties-fb
  ;; values = :TYPE-RENDER-MODEL-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (vendor-id :uint32)
  ;; length = +MAX-RENDER-MODEL-NAME-SIZE-FB+
  (model-name :char :count 64)
  (model-key render-model-key-fb)
  (model-version :uint32)
  (flags render-model-flags-fb))

(defcstruct render-model-buffer-fb
  ;; values = :TYPE-RENDER-MODEL-BUFFER-FB
  (type structure-type)
  (next (:pointer :void))
  (buffer-capacity-input :uint32) ;; optional
  (buffer-count-output :uint32) ;; optional
  ;; length = buffer-capacity-input
  (buffer (:pointer :uint8)) ;; optional
)

(defcstruct render-model-load-info-fb
  ;; values = :TYPE-RENDER-MODEL-LOAD-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (model-key render-model-key-fb))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-render-model-properties-fb
  ;; values = :TYPE-SYSTEM-RENDER-MODEL-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-render-model-loading bool-32))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct render-model-capabilities-request-fb
  ;; values = :TYPE-RENDER-MODEL-CAPABILITIES-REQUEST-FB
  (type structure-type)
  (next (:pointer :void))
  (flags render-model-flags-fb))

;; XR_FB_spatial_entity_query structs
(defcstruct space-query-info-base-header-fb
  (type structure-type)
  (next (:pointer :void)))

(defcstruct space-filter-info-base-header-fb
  (type structure-type)
  (next (:pointer :void)))

;;  parent : SPACE-QUERY-INFO-BASE-HEADER-FB
(defcstruct space-query-info-fb
  ;; values = :TYPE-SPACE-QUERY-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (query-action space-query-action-fb)
  (max-result-count :uint32)
  (timeout duration)
  (filter (:pointer (:struct space-filter-info-base-header-fb))) ;; optional
  (exclude-filter (:pointer (:struct space-filter-info-base-header-fb))) ;; optional
)

;;  extends : SPACE-FILTER-INFO-BASE-HEADER-FB
(defcstruct space-storage-location-filter-info-fb
  ;; values = :TYPE-SPACE-STORAGE-LOCATION-FILTER-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (location space-storage-location-fb))

;;  parent : SPACE-FILTER-INFO-BASE-HEADER-FB
(defcstruct space-uuid-filter-info-fb
  ;; values = :TYPE-SPACE-UUID-FILTER-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (uuid-count :uint32)
  ;; length = uuid-count
  (uuids (:pointer (:struct uuid-ext))))

;;  parent : SPACE-FILTER-INFO-BASE-HEADER-FB
(defcstruct space-component-filter-info-fb
  ;; values = :TYPE-SPACE-COMPONENT-FILTER-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (component-type space-component-type-fb))

;;  returned only : true
(defcstruct space-query-result-fb
  (space space)
  (uuid (:struct uuid-ext)))

(defcstruct space-query-results-fb
  ;; values = :TYPE-SPACE-QUERY-RESULTS-FB
  (type structure-type)
  (next (:pointer :void))
  (result-capacity-input :uint32) ;; optional
  (result-count-output :uint32) ;; optional
  ;; length = result-capacity-input
  (results (:pointer (:struct space-query-result-fb))) ;; optional
)

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-query-results-available-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-QUERY-RESULTS-AVAILABLE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-query-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-QUERY-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result))

;; XR_FB_spatial_entity_storage structs
(defcstruct space-save-info-fb
  ;; values = :TYPE-SPACE-SAVE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (space space)
  (location space-storage-location-fb)
  (persistence-mode space-persistence-mode-fb))

(defcstruct space-erase-info-fb
  ;; values = :TYPE-SPACE-ERASE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (space space)
  (location space-storage-location-fb))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-save-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-SAVE-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext))
  (location space-storage-location-fb))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-erase-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-ERASE-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext))
  (location space-storage-location-fb))

;; XR_FB_spatial_entity_sharing structs
(defcstruct space-share-info-fb
  ;; values = :TYPE-SPACE-SHARE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (space-count :uint32)
  ;; length = space-count
  (spaces (:pointer space))
  (user-count :uint32)
  ;; length = user-count
  (users (:pointer space-user-fb)))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-share-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-SHARE-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result))

;; XR_FB_spatial_entity_storage_batch structs
(defcstruct space-list-save-info-fb
  ;; values = :TYPE-SPACE-LIST-SAVE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (space-count :uint32)
  ;; length = space-count
  (spaces (:pointer space))
  (location space-storage-location-fb))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-space-list-save-complete-fb
  ;; values = :TYPE-EVENT-DATA-SPACE-LIST-SAVE-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result))

;; XR_FB_spatial_entity_container structs
(defcstruct space-container-fb
  ;; values = :TYPE-SPACE-CONTAINER-FB
  (type structure-type)
  (next (:pointer :void))
  (uuid-capacity-input :uint32) ;; optional
  (uuid-count-output :uint32) ;; optional
  ;; length = uuid-capacity-input
  (uuids (:pointer (:struct uuid-ext))) ;; optional
)

;; XR_FB_scene structs
(defcstruct extent-3d-f-fb
  (width :float)
  (height :float)
  (depth :float))

(defcstruct offset-3d-f-fb
  (x :float)
  (y :float)
  (z :float))

(defcstruct rect-3d-f-fb
  (offset (:struct offset-3d-f-fb))
  (extent (:struct extent-3d-f-fb)))

(defcstruct semantic-labels-fb
  ;; values = :TYPE-SEMANTIC-LABELS-FB
  (type structure-type)
  (next (:pointer :void))
  (buffer-capacity-input :uint32) ;; optional
  (buffer-count-output :uint32) ;; optional
  ;; length = buffer-capacity-input
  (buffer :string) ;; optional
)

(defcstruct room-layout-fb
  ;; values = :TYPE-ROOM-LAYOUT-FB
  (type structure-type)
  (next (:pointer :void))
  (floor-uuid (:struct uuid-ext))
  (ceiling-uuid (:struct uuid-ext))
  (wall-uuid-capacity-input :uint32) ;; optional
  (wall-uuid-count-output :uint32) ;; optional
  ;; length = wall-uuid-capacity-input
  (wall-uuids (:pointer (:struct uuid-ext))) ;; optional
)

(defcstruct boundary-2d-fb
  ;; values = :TYPE-BOUNDARY-2D-FB
  (type structure-type)
  (next (:pointer :void))
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32) ;; optional
  ;; length = vertex-capacity-input
  (vertices (:pointer (:struct vector-2f))) ;; optional
)

;; XR_FB_scene_capture structs
(defcstruct scene-capture-request-info-fb
  ;; values = :TYPE-SCENE-CAPTURE-REQUEST-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (request-byte-count :uint32) ;; optional
  ;; length = request-byte-count
  (request :string) ;; optional
)

(defcstruct event-data-scene-capture-complete-fb
  ;; values = :TYPE-EVENT-DATA-SCENE-CAPTURE-COMPLETE-FB
  (type structure-type)
  (next (:pointer :void))
  (request-id async-request-id-fb)
  (result result))

;; XR_FB_keyboard_tracking structs
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-keyboard-tracking-properties-fb
  ;; values = :TYPE-SYSTEM-KEYBOARD-TRACKING-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-keyboard-tracking bool-32))

;;  returned only : true
(defcstruct keyboard-tracking-description-fb
  (tracked-keyboard-id :uint64)
  (size (:struct vector-3f))
  (flags keyboard-tracking-flags-fb)
  ;; length = +MAX-KEYBOARD-TRACKING-NAME-SIZE-FB+
  (name :char :count 128))

(defcstruct keyboard-space-create-info-fb
  ;; values = :TYPE-KEYBOARD-SPACE-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (tracked-keyboard-id :uint64))

(defcstruct keyboard-tracking-query-fb
  ;; values = :TYPE-KEYBOARD-TRACKING-QUERY-FB
  (type structure-type)
  (next (:pointer :void))
  (flags keyboard-tracking-query-flags-fb))

;; XR_VARJO_composition_layer_depth_test
;;  extends : COMPOSITION-LAYER-PROJECTION
(defcstruct composition-layer-depth-test-varjo
  ;; values = :TYPE-COMPOSITION-LAYER-DEPTH-TEST-VARJO
  (type structure-type)
  (next (:pointer :void))
  (depth-test-range-near-z :float)
  (depth-test-range-far-z :float))

;; XR_VARJO_foveated_rendering
;;  extends : VIEW-LOCATE-INFO
(defcstruct view-locate-foveated-rendering-varjo
  ;; values = :TYPE-VIEW-LOCATE-FOVEATED-RENDERING-VARJO
  (type structure-type)
  (next (:pointer :void))
  (foveated-rendering-active bool-32))

;;  extends : VIEW-CONFIGURATION-VIEW
(defcstruct foveated-view-configuration-view-varjo
  ;; values = :TYPE-FOVEATED-VIEW-CONFIGURATION-VIEW-VARJO
  (type structure-type)
  (next (:pointer :void))
  (foveated-rendering-active bool-32))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-foveated-rendering-properties-varjo
  ;; values = :TYPE-SYSTEM-FOVEATED-RENDERING-PROPERTIES-VARJO
  (type structure-type)
  (next (:pointer :void))
  (supports-foveated-rendering bool-32))

;;  extends : COMPOSITION-LAYER-PROJECTION
(defcstruct composition-layer-reprojection-info-msft
  ;; values = :TYPE-COMPOSITION-LAYER-REPROJECTION-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (reprojection-mode reprojection-mode-msft))

;;  extends : COMPOSITION-LAYER-PROJECTION
(defcstruct composition-layer-reprojection-plane-override-msft
  ;; values = :TYPE-COMPOSITION-LAYER-REPROJECTION-PLANE-OVERRIDE-MSFT
  (type structure-type)
  (next (:pointer :void))
  (position (:struct vector-3f))
  (normal (:struct vector-3f))
  (velocity (:struct vector-3f)))

;; XR_FB_triangle_mesh
(defcstruct triangle-mesh-create-info-fb
  ;; values = :TYPE-TRIANGLE-MESH-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (flags triangle-mesh-flags-fb) ;; optional
  (winding-order winding-order-fb)
  (vertex-count :uint32)
  (vertex-buffer (:pointer (:struct vector-3f))) ;; optional, noautovalidity
  (triangle-count :uint32)
  (index-buffer (:pointer :uint32)) ;; optional, noautovalidity
)

;; XR_FB_passthrough
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-passthrough-properties-fb
  ;; values = :TYPE-SYSTEM-PASSTHROUGH-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (supports-passthrough bool-32))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-passthrough-properties-2-fb
  ;; values = :TYPE-SYSTEM-PASSTHROUGH-PROPERTIES2-FB
  (type structure-type)
  (next (:pointer :void))
  (capabilities passthrough-capability-flags-fb))

(defcstruct passthrough-create-info-fb
  ;; values = :TYPE-PASSTHROUGH-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (flags passthrough-flags-fb))

(defcstruct passthrough-layer-create-info-fb
  ;; values = :TYPE-PASSTHROUGH-LAYER-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (passthrough passthrough-fb)
  (flags passthrough-flags-fb)
  (purpose passthrough-layer-purpose-fb))

;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-passthrough-fb
  ;; values = :TYPE-COMPOSITION-LAYER-PASSTHROUGH-FB
  (type structure-type)
  (next (:pointer :void))
  (flags composition-layer-flags)
  (space space)
  (layer-handle passthrough-layer-fb))

(defcstruct geometry-instance-create-info-fb
  ;; values = :TYPE-GEOMETRY-INSTANCE-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (layer passthrough-layer-fb)
  (mesh triangle-mesh-fb)
  (base-space space)
  (pose (:struct pose-f))
  (scale (:struct vector-3f)))

(defcstruct geometry-instance-transform-fb
  ;; values = :TYPE-GEOMETRY-INSTANCE-TRANSFORM-FB
  (type structure-type)
  (next (:pointer :void))
  (base-space space)
  (time time)
  (pose (:struct pose-f))
  (scale (:struct vector-3f)))

(defcstruct passthrough-style-fb
  ;; values = :TYPE-PASSTHROUGH-STYLE-FB
  (type structure-type)
  (next (:pointer :void))
  (texture-opacity-factor :float)
  (edge-color (:struct color-4f)))

;;  extends : PASSTHROUGH-STYLE-FB
(defcstruct passthrough-color-map-mono-to-rgba-fb
  ;; values = :TYPE-PASSTHROUGH-COLOR-MAP-MONO-TO-RGBA-FB
  (type structure-type)
  (next (:pointer :void))
  ;; length = +PASSTHROUGH-COLOR-MAP-MONO-SIZE-FB+
  (texture-color-map (:struct color-4f) :count 256))

;;  extends : PASSTHROUGH-STYLE-FB
(defcstruct passthrough-color-map-mono-to-mono-fb
  ;; values = :TYPE-PASSTHROUGH-COLOR-MAP-MONO-TO-MONO-FB
  (type structure-type)
  (next (:pointer :void))
  ;; length = +PASSTHROUGH-COLOR-MAP-MONO-SIZE-FB+
  (texture-color-map :uint8 :count 256))

;;  extends : PASSTHROUGH-STYLE-FB
(defcstruct passthrough-brightness-contrast-saturation-fb
  ;; values = :TYPE-PASSTHROUGH-BRIGHTNESS-CONTRAST-SATURATION-FB
  (type structure-type)
  (next (:pointer :void))
  (brightness :float)
  (contrast :float)
  (saturation :float))

(defcstruct event-data-passthrough-state-changed-fb
  ;; values = :TYPE-EVENT-DATA-PASSTHROUGH-STATE-CHANGED-FB
  (type structure-type)
  (next (:pointer :void))
  (flags passthrough-state-changed-flags-fb))

;; XR_FB_passthrough_keyboard_hands
(defcstruct passthrough-keyboard-hands-intensity-fb
  ;; values = :TYPE-PASSTHROUGH-KEYBOARD-HANDS-INTENSITY-FB
  (type structure-type)
  (next (:pointer :void))
  (left-hand-intensity :float)
  (right-hand-intensity :float))

;; XR_META_local_dimming
;;  extends : FRAME-END-INFO
(defcstruct local-dimming-frame-end-info-meta
  ;; values = :TYPE-LOCAL-DIMMING-FRAME-END-INFO-META
  (type structure-type)
  (next (:pointer :void))
  (local-dimming-mode local-dimming-mode-meta))

(defcstruct spatial-anchor-persistence-name-msft
  ;; length = +MAX-SPATIAL-ANCHOR-NAME-SIZE-MSFT+
  (name :char :count 256))

(defcstruct spatial-anchor-persistence-info-msft
  ;; values = :TYPE-SPATIAL-ANCHOR-PERSISTENCE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (spatial-anchor-persistence-name (:struct
                                    spatial-anchor-persistence-name-msft))
  (spatial-anchor spatial-anchor-msft))

(defcstruct spatial-anchor-from-persisted-anchor-create-info-msft
  ;; values = :TYPE-SPATIAL-ANCHOR-FROM-PERSISTED-ANCHOR-CREATE-INFO-MSFT
  (type structure-type)
  (next (:pointer :void))
  (spatial-anchor-store spatial-anchor-store-connection-msft)
  (spatial-anchor-persistence-name (:struct
                                    spatial-anchor-persistence-name-msft)))

;; XR_HTC_facial_tracking
(defcstruct facial-tracker-create-info-htc
  ;; values = :TYPE-FACIAL-TRACKER-CREATE-INFO-HTC
  (type structure-type)
  (next (:pointer :void))
  (facial-tracking-type facial-tracking-type-htc))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-facial-tracking-properties-htc
  ;; values = :TYPE-SYSTEM-FACIAL-TRACKING-PROPERTIES-HTC
  (type structure-type)
  (next (:pointer :void))
  (support-eye-facial-tracking bool-32)
  (support-lip-facial-tracking bool-32))

(defcstruct facial-expressions-htc
  ;; values = :TYPE-FACIAL-EXPRESSIONS-HTC
  (type structure-type)
  (next (:pointer :void))
  (is-active bool-32)
  (sample-time time)
  (expression-count :uint32)
  (expression-weightings (:pointer :float)))

;; XR_HTC_passthrough
(defcstruct passthrough-create-info-htc
  ;; values = :TYPE-PASSTHROUGH-CREATE-INFO-HTC
  (type structure-type)
  (next (:pointer :void))
  (form passthrough-form-htc))

(defcstruct passthrough-color-htc
  ;; values = :TYPE-PASSTHROUGH-COLOR-HTC
  (type structure-type)
  (next (:pointer :void))
  (alpha :float))

;;  extends : COMPOSITION-LAYER-PASSTHROUGH-HTC
(defcstruct passthrough-mesh-transform-info-htc
  ;; values = :TYPE-PASSTHROUGH-MESH-TRANSFORM-INFO-HTC
  (type structure-type)
  (next (:pointer :void))
  (vertex-count :uint32)
  ;; length = vertex-count
  (vertices (:pointer (:struct vector-3f)))
  (index-count :uint32)
  ;; length = index-count
  (indices (:pointer :uint32))
  (base-space space)
  (time time)
  (pose (:struct pose-f))
  (scale (:struct vector-3f)))

;;  parent : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-passthrough-htc
  ;; values = :TYPE-COMPOSITION-LAYER-PASSTHROUGH-HTC
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-flags)
  (space space)
  (passthrough passthrough-htc)
  (color (:struct passthrough-color-htc)))

;; XR_HTCX_vive_tracker_interaction structs
;;  returned only : true
(defcstruct vive-tracker-paths-htcx
  ;; values = :TYPE-VIVE-TRACKER-PATHS-HTCX
  (type structure-type)
  (next (:pointer :void))
  (persistent-path path)
  (role-path path) ;; optional
)

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-vive-tracker-connected-htcx
  ;; values = :TYPE-EVENT-DATA-VIVE-TRACKER-CONNECTED-HTCX
  (type structure-type)
  (next (:pointer :void))
  (paths (:pointer (:struct vive-tracker-paths-htcx))))

;; XR_FB_space_warp
;;  extends : COMPOSITION-LAYER-PROJECTION-VIEW
(defcstruct composition-layer-space-warp-info-fb
  ;; values = :TYPE-COMPOSITION-LAYER-SPACE-WARP-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-space-warp-info-flags-fb) ;; optional
  (motion-vector-sub-image (:struct swapchain-sub-image))
  (app-space-delta-pose (:struct pose-f))
  (depth-sub-image (:struct swapchain-sub-image))
  (min-depth :float)
  (max-depth :float)
  (near-z :float)
  (far-z :float))

;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-space-warp-properties-fb
  ;; values = :TYPE-SYSTEM-SPACE-WARP-PROPERTIES-FB
  (type structure-type)
  (next (:pointer :void))
  (recommended-motion-vector-image-rect-width :uint32)
  (recommended-motion-vector-image-rect-height :uint32))

;; XR_VARJO_marker_tracking
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-marker-tracking-properties-varjo
  ;; values = :TYPE-SYSTEM-MARKER-TRACKING-PROPERTIES-VARJO
  (type structure-type)
  (next (:pointer :void))
  (supports-marker-tracking bool-32))

;;  parent : EVENT-DATA-BASE-HEADER
;;  returned only : true
(defcstruct event-data-marker-tracking-update-varjo
  ;; values = :TYPE-EVENT-DATA-MARKER-TRACKING-UPDATE-VARJO
  (type structure-type)
  (next (:pointer :void))
  (marker-id :uint64)
  (is-active bool-32)
  (is-predicted bool-32)
  (time time))

(defcstruct marker-space-create-info-varjo
  ;; values = :TYPE-MARKER-SPACE-CREATE-INFO-VARJO
  (type structure-type)
  (next (:pointer :void))
  (marker-id :uint64)
  (pose-in-marker-space (:struct pose-f)))

;; XR_ML_global_dimmer
;;  extends : FRAME-END-INFO
(defcstruct global-dimmer-frame-end-info-ml
  ;; values = :TYPE-GLOBAL-DIMMER-FRAME-END-INFO-ML
  (type structure-type)
  (next (:pointer :void))
  (dimmer-value :float)
  (flags global-dimmer-frame-end-info-flags-ml) ;; optional
)

;; XR_ALMALENCE_digital_lens_control
(defcstruct digital-lens-control-almalence
  ;; values = :TYPE-DIGITAL-LENS-CONTROL-ALMALENCE
  (type structure-type)
  (next (:pointer :void))
  (flags digital-lens-control-flags-almalence))

;; XR_FB_composition_layer_settings structs
;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-settings-fb
  ;; values = :TYPE-COMPOSITION-LAYER-SETTINGS-FB
  (type structure-type)
  (next (:pointer :void))
  (layer-flags composition-layer-settings-flags-fb))

;; XR_OCULUS_external_camera
(defcstruct external-camera-intrinsics-oculus
  (last-change-time time)
  (fov (:struct fov-f))
  (virtual-near-plane-distance :float)
  (virtual-far-plane-distance :float)
  (image-sensor-pixel-resolution (:struct extent-2d-i)))

(defcstruct external-camera-extrinsics-oculus
  (last-change-time time)
  (camera-status-flags external-camera-status-flags-oculus) ;; optional
  (attached-to-device external-camera-attached-to-device-oculus)
  (relative-pose (:struct pose-f)))

;;  returned only : true
(defcstruct external-camera-oculus
  ;; values = :TYPE-EXTERNAL-CAMERA-OCULUS
  (type structure-type)
  (next (:pointer :void))
  ;; length = +MAX-EXTERNAL-CAMERA-NAME-SIZE-OCULUS+
  (name :char :count 32)
  (intrinsics (:struct external-camera-intrinsics-oculus))
  (extrinsics (:struct external-camera-extrinsics-oculus)))

;; XR_META_performance_metrics
(defcstruct performance-metrics-state-meta
  ;; values = :TYPE-PERFORMANCE-METRICS-STATE-META
  (type structure-type)
  (next (:pointer :void))
  (enabled bool-32))

(defcstruct performance-metrics-counter-meta
  ;; values = :TYPE-PERFORMANCE-METRICS-COUNTER-META
  (type structure-type)
  (next (:pointer :void))
  (counter-flags performance-metrics-counter-flags-meta) ;; optional
  (counter-unit performance-metrics-counter-unit-meta)
  (uint-value :uint32)
  (float-value :float))

;; XR_META_headset_id structs
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-headset-id-properties-meta
  ;; values = :TYPE-SYSTEM-HEADSET-ID-PROPERTIES-META
  (type structure-type)
  (next (:pointer :void))
  (id (:struct uuid-ext)))

;; XR_HTC_foveation structs
(defcstruct foveation-apply-info-htc
  ;; values = :TYPE-FOVEATION-APPLY-INFO-HTC
  (type structure-type)
  (next (:pointer :void))
  (mode foveation-mode-htc)
  (sub-image-count :uint32)
  ;; length = sub-image-count
  (sub-images (:pointer (:struct swapchain-sub-image))))

(defcstruct foveation-configuration-htc
  (level foveation-level-htc)
  (clear-fov-degree :float)
  (focal-center-offset (:struct vector-2f)))

;;  extends : FOVEATION-APPLY-INFO-HTC
(defcstruct foveation-dynamic-mode-info-htc
  ;; values = :TYPE-FOVEATION-DYNAMIC-MODE-INFO-HTC
  (type structure-type)
  (next (:pointer :void))
  (dynamic-flags foveation-dynamic-flags-htc) ;; optional
)

;;  extends : FOVEATION-APPLY-INFO-HTC
(defcstruct foveation-custom-mode-info-htc
  ;; values = :TYPE-FOVEATION-CUSTOM-MODE-INFO-HTC
  (type structure-type)
  (next (:pointer :void))
  (config-count :uint32)
  ;; length = config-count
  (configs (:pointer (:struct foveation-configuration-htc))))

(defcstruct active-action-set-priority-ext
  (action-set action-set)
  (priority-override :uint32))

;; XR_EXT_active_action_set_priority
;;  extends : ACTIONS-SYNC-INFO
(defcstruct active-action-set-priorities-ext
  ;; values = :TYPE-ACTIVE-ACTION-SET-PRIORITIES-EXT
  (type structure-type)
  (next (:pointer :void))
  (action-set-priority-count :uint32)
  ;; length = action-set-priority-count
  (action-set-priorities (:pointer (:struct active-action-set-priority-ext))))

;; XR_FB_composition_layer_depth_test
;;  extends : COMPOSITION-LAYER-BASE-HEADER
(defcstruct composition-layer-depth-test-fb
  ;; values = :TYPE-COMPOSITION-LAYER-DEPTH-TEST-FB
  (type structure-type)
  (next (:pointer :void))
  (depth-mask bool-32)
  (compare-op compare-op-fb))

;; XR_ML_compat structs
(defcstruct coordinate-space-create-info-ml
  ;; values = :TYPE-COORDINATE-SPACE-CREATE-INFO-ML
  (type structure-type)
  (next (:pointer :void))
  (cfuid ml-coordinate-frame-uid)
  (pose-in-coordinate-space (:struct pose-f)))

;; XR_ML_frame_end_info structs
;;  extends : FRAME-END-INFO
(defcstruct frame-end-info-ml
  ;; values = :TYPE-FRAME-END-INFO-ML
  (type structure-type)
  (next (:pointer :void))
  (focus-distance :float)
  (flags frame-end-info-flags-ml) ;; optional
)

;; XR_FB_haptic_amplitude_envelope struct
;;  parent : HAPTIC-BASE-HEADER
(defcstruct haptic-amplitude-envelope-vibration-fb
  ;; values = :TYPE-HAPTIC-AMPLITUDE-ENVELOPE-VIBRATION-FB
  (type structure-type)
  (next (:pointer :void))
  (duration duration)
  (amplitude-count :uint32)
  ;; length = amplitude-count
  (amplitudes (:pointer :float)))

;; XR_FB_haptic_pcm structs
;;  parent : HAPTIC-BASE-HEADER
(defcstruct haptic-pcm-vibration-fb
  ;; values = :TYPE-HAPTIC-PCM-VIBRATION-FB
  (type structure-type)
  (next (:pointer :void))
  (buffer-size :uint32)
  ;; length = buffer-size
  (buffer (:pointer :float))
  (sample-rate :float)
  (append bool-32)
  (samples-consumed (:pointer :uint32)))

(defcstruct device-pcm-sample-rate-state-fb
  ;; values = :TYPE-DEVICE-PCM-SAMPLE-RATE-STATE-FB
  (type structure-type)
  (next (:pointer :void))
  (sample-rate :float))

;; alias struct DEVICE-PCM-SAMPLE-RATE-GET-INFO-FB
;;           -> DEVICE-PCM-SAMPLE-RATE-STATE-FB
(defcstruct device-pcm-sample-rate-get-info-fb
  ;; values = :TYPE-DEVICE-PCM-SAMPLE-RATE-STATE-FB
  (type structure-type)
  (next (:pointer :void))
  (sample-rate :float))

;; XR_FB_spatial_entity_user
(defcstruct space-user-create-info-fb
  ;; values = :TYPE-SPACE-USER-CREATE-INFO-FB
  (type structure-type)
  (next (:pointer :void))
  (user-id space-user-id-fb))

;; XR_MNDX_force_feedback_curl
;;  extends : SYSTEM-PROPERTIES
;;  returned only : true
(defcstruct system-force-feedback-curl-properties-mndx
  ;; values = :TYPE-SYSTEM-FORCE-FEEDBACK-CURL-PROPERTIES-MNDX
  (type structure-type)
  (next (:pointer :void))
  (supports-force-feedback-curl bool-32))

(defcstruct force-feedback-curl-apply-location-mndx
  (location force-feedback-curl-location-mndx)
  (value :float))

(defcstruct force-feedback-curl-apply-locations-mndx
  ;; values = :TYPE-FORCE-FEEDBACK-CURL-APPLY-LOCATIONS-MNDX
  (type structure-type)
  (next (:pointer :void))
  (location-count :uint32)
  ;; length = location-count
  (locations (:pointer (:struct force-feedback-curl-apply-location-mndx))))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-out-of-memory)
(defcfun ("xrGetInstanceProcAddr" get-instance-proc-addr) result
  ;; optional = "true"
  (instance instance)
  ;; length = :null-terminated
  (name :string)
  (function (:pointer pfn-void-function)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-out-of-memory :error-size-insufficient)
(defcfun ("xrEnumerateApiLayerProperties" enumerate-api-layer-properties) result
  ;; optional = "true"
  (property-capacity-input :uint32)
  (property-count-output (:pointer :uint32))
  ;; length = property-capacity-input
  ;; optional = "true"
  (properties (:pointer (:struct api-layer-properties))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-out-of-memory :error-size-insufficient
;;                 :error-runtime-unavailable :error-api-layer-not-present)
(defcfun ("xrEnumerateInstanceExtensionProperties" enumerate-instance-extension-properties) result
  ;; length = :null-terminated
  ;; optional = "true"
  (layer-name :string)
  ;; optional = "true"
  (property-capacity-input :uint32)
  (property-count-output (:pointer :uint32))
  ;; length = property-capacity-input
  ;; optional = "true"
  (properties (:pointer (:struct extension-properties))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-out-of-memory :error-limit-reached
;;                 :error-runtime-unavailable :error-name-invalid
;;                 :error-initialization-failed :error-extension-not-present
;;                 :error-api-version-unsupported :error-api-layer-not-present)
(defcfun ("xrCreateInstance" create-instance) result
  (create-info (:pointer (:struct instance-create-info)))
  (instance (:pointer instance)))

;; success codes: (:success)
;;   error codes: (:error-handle-invalid)
(defcfun ("xrDestroyInstance" destroy-instance) result
  ;; externsync = "true_with_children"
  (instance instance))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost)
(defcfun ("xrResultToString" result-to-string) result
  (instance instance)
  (value result)
  ;; length = +MAX-RESULT-STRING-SIZE+ (64)
  (buffer (:pointer :char)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost)
(defcfun ("xrStructureTypeToString" structure-type-to-string) result
  (instance instance)
  (value structure-type)
  ;; length = +MAX-STRUCTURE-NAME-SIZE+ (64)
  (buffer (:pointer :char)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost)
(defcfun ("xrGetInstanceProperties" get-instance-properties) result
  (instance instance)
  (instance-properties (:pointer (:struct instance-properties))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-form-factor-unsupported :error-form-factor-unavailable)
(defcfun ("xrGetSystem" get-system) result
  (instance instance)
  (get-info (:pointer (:struct system-get-info)))
  (system-id (:pointer system-id)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-out-of-memory
;;                 :error-system-invalid)
(defcfun ("xrGetSystemProperties" get-system-properties) result
  (instance instance)
  (system-id system-id)
  (properties (:pointer (:struct system-properties))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-out-of-memory
;;                 :error-limit-reached :error-system-invalid
;;                 :error-initialization-failed
;;                 :error-graphics-requirements-call-missing
;;                 :error-graphics-device-invalid)
(defcfun ("xrCreateSession" create-session) result
  (instance instance)
  (create-info (:pointer (:struct session-create-info)))
  (session (:pointer session)))

;; success codes: (:success)
;;   error codes: (:error-handle-invalid)
(defcfun ("xrDestroySession" destroy-session) result
  ;; externsync = "true_with_children"
  (session session))

;; success codes: (:success)
;;   error codes: (:error-handle-invalid)
(defcfun ("xrDestroySpace" destroy-space) result
  ;; externsync = "true_with_children"
  (space space))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-size-insufficient)
(defcfun ("xrEnumerateSwapchainFormats" enumerate-swapchain-formats) result
  (session session)
  ;; optional = "true"
  (format-capacity-input :uint32)
  (format-count-output (:pointer :uint32))
  ;; length = format-capacity-input
  ;; optional = "true"
  (formats (:pointer :int64)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached
;;                 :error-swapchain-format-unsupported :error-feature-unsupported)
(defcfun ("xrCreateSwapchain" create-swapchain) result
  (session session)
  (create-info (:pointer (:struct swapchain-create-info)))
  (swapchain (:pointer swapchain)))

;; success codes: (:success)
;;   error codes: (:error-handle-invalid)
(defcfun ("xrDestroySwapchain" destroy-swapchain) result
  ;; externsync = "true_with_children"
  (swapchain swapchain))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-size-insufficient)
(defcfun ("xrEnumerateSwapchainImages" enumerate-swapchain-images) result
  (swapchain swapchain)
  ;; optional = "true"
  (image-capacity-input :uint32)
  (image-count-output (:pointer :uint32))
  ;; length = image-capacity-input
  ;; optional = "true"
  (images (:pointer (:struct swapchain-image-base-header))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-call-order-invalid)
(defcfun ("xrAcquireSwapchainImage" acquire-swapchain-image) result
  (swapchain swapchain)
  ;; optional = "true"
  (acquire-info (:pointer (:struct swapchain-image-acquire-info)))
  (index (:pointer :uint32)))

;; success codes: (:success :session-loss-pending :timeout-expired)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-call-order-invalid)
(defcfun ("xrWaitSwapchainImage" wait-swapchain-image) result
  (swapchain swapchain)
  (wait-info (:pointer (:struct swapchain-image-wait-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-call-order-invalid)
(defcfun ("xrReleaseSwapchainImage" release-swapchain-image) result
  (swapchain swapchain)
  ;; optional = "true"
  (release-info (:pointer (:struct swapchain-image-release-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-view-configuration-type-unsupported
;;                 :error-session-running :error-session-not-ready)
(defcfun ("xrBeginSession" begin-session) result
  (session session)
  (begin-info (:pointer (:struct session-begin-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-session-not-stopping :error-session-not-running)
(defcfun ("xrEndSession" end-session) result
  (session session))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-session-not-running)
(defcfun ("xrRequestExitSession" request-exit-session) result
  (session session))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-size-insufficient)
(defcfun ("xrEnumerateReferenceSpaces" enumerate-reference-spaces) result
  (session session)
  ;; optional = "true"
  (space-capacity-input :uint32)
  (space-count-output (:pointer :uint32))
  ;; length = space-capacity-input
  ;; optional = "true"
  (spaces (:pointer reference-space-type)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached
;;                 :error-reference-space-unsupported :error-pose-invalid)
(defcfun ("xrCreateReferenceSpace" create-reference-space) result
  (session session)
  (create-info (:pointer (:struct reference-space-create-info)))
  (space (:pointer space)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached :error-pose-invalid
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch)
(defcfun ("xrCreateActionSpace" create-action-space) result
  (session session)
  (create-info (:pointer (:struct action-space-create-info)))
  (space (:pointer space)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-time-invalid)
(defcfun ("xrLocateSpace" locate-space) result
  (space space)
  (base-space space)
  (time time)
  (location (:pointer (:struct space-location))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-size-insufficient :error-system-invalid)
(defcfun ("xrEnumerateViewConfigurations" enumerate-view-configurations) result
  (instance instance)
  (system-id system-id)
  ;; optional = "true"
  (view-configuration-type-capacity-input :uint32)
  (view-configuration-type-count-output (:pointer :uint32))
  ;; length = view-configuration-type-capacity-input
  ;; optional = "true"
  (view-configuration-types (:pointer view-configuration-type)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-size-insufficient
;;                 :error-view-configuration-type-unsupported
;;                 :error-system-invalid)
(defcfun ("xrEnumerateEnvironmentBlendModes" enumerate-environment-blend-modes) result
  (instance instance)
  (system-id system-id)
  (view-configuration-type view-configuration-type)
  ;; optional = "true"
  (environment-blend-mode-capacity-input :uint32)
  (environment-blend-mode-count-output (:pointer :uint32))
  ;; length = environment-blend-mode-capacity-input
  ;; optional = "true"
  (environment-blend-modes (:pointer environment-blend-mode)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-view-configuration-type-unsupported
;;                 :error-system-invalid)
(defcfun ("xrGetViewConfigurationProperties" get-view-configuration-properties) result
  (instance instance)
  (system-id system-id)
  (view-configuration-type view-configuration-type)
  (configuration-properties (:pointer (:struct view-configuration-properties))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-size-insufficient
;;                 :error-view-configuration-type-unsupported
;;                 :error-system-invalid)
(defcfun ("xrEnumerateViewConfigurationViews" enumerate-view-configuration-views) result
  (instance instance)
  (system-id system-id)
  (view-configuration-type view-configuration-type)
  ;; optional = "true"
  (view-capacity-input :uint32)
  (view-count-output (:pointer :uint32))
  ;; length = view-capacity-input
  ;; optional = "true"
  (views (:pointer (:struct view-configuration-view))))

;; success codes: (:success :session-loss-pending :frame-discarded)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-session-not-running :error-call-order-invalid)
(defcfun ("xrBeginFrame" begin-frame) result
  ;; Implicit external sync: the SESSION parameter by any other BEGIN-FRAME call or END-FRAME call
  (session session)
  ;; optional = "true"
  (frame-begin-info (:pointer (:struct frame-begin-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-size-insufficient
;;                 :error-view-configuration-type-unsupported :error-time-invalid)
(defcfun ("xrLocateViews" locate-views) result
  (session session)
  (view-locate-info (:pointer (:struct view-locate-info)))
  (view-state (:pointer (:struct view-state)))
  ;; optional = "true"
  (view-capacity-input :uint32)
  (view-count-output (:pointer :uint32))
  ;; length = view-capacity-input
  ;; optional = "true"
  (views (:pointer (:struct view))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-time-invalid :error-swapchain-rect-invalid
;;                 :error-session-not-running :error-pose-invalid
;;                 :error-layer-limit-exceeded :error-layer-invalid
;;                 :error-environment-blend-mode-unsupported
;;                 :error-call-order-invalid)
(defcfun ("xrEndFrame" end-frame) result
  ;; Implicit external sync: the SESSION parameter by any other BEGIN-FRAME call or END-FRAME call
  (session session)
  (frame-end-info (:pointer (:struct frame-end-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-session-not-running)
(defcfun ("xrWaitFrame" wait-frame) result
  ;; Implicit external sync: the SESSION parameter by any other WAIT-FRAME call
  (session session)
  ;; optional = "true"
  (frame-wait-info (:pointer (:struct frame-wait-info)))
  (frame-state (:pointer (:struct frame-state))))

;; success codes: (:success :session-loss-pending :session-not-focused)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defcfun ("xrApplyHapticFeedback" apply-haptic-feedback) result
  (session session)
  (haptic-action-info (:pointer (:struct haptic-action-info)))
  (haptic-feedback (:pointer (:struct haptic-base-header))))

;; success codes: (:success :session-loss-pending :session-not-focused)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defcfun ("xrStopHapticFeedback" stop-haptic-feedback) result
  (session session)
  (haptic-action-info (:pointer (:struct haptic-action-info))))

;; success codes: (:success :event-unavailable)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost)
(defcfun ("xrPollEvent" poll-event) result
  (instance instance)
  (event-data (:pointer (:struct event-data-buffer))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-path-format-invalid :error-path-count-exceeded)
(defcfun ("xrStringToPath" string-to-path) result
  (instance instance)
  ;; length = :null-terminated
  (path-string :string)
  (path (:pointer path)))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-size-insufficient :error-path-invalid)
(defcfun ("xrPathToString" path-to-string) result
  (instance instance)
  (path path)
  ;; optional = "true"
  (buffer-capacity-input :uint32)
  (buffer-count-output (:pointer :uint32))
  ;; length = buffer-capacity-input
  ;; optional = "true"
  (buffer :string))

;; success codes: (:success :session-loss-pending :space-bounds-unavailable)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-reference-space-unsupported)
(defcfun ("xrGetReferenceSpaceBoundsRect" get-reference-space-bounds-rect) result
  (session session)
  (reference-space-type reference-space-type)
  (bounds (:pointer (:struct extent-2d-f))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-android-thread-settings-id-invalid-khr
;;                 :error-android-thread-settings-failure-khr)
(defextfun ("xrSetAndroidApplicationThreadKHR" set-android-application-thread-khr 0) result
  (session session)
  (thread-type android-thread-type-khr)
  (thread-id :uint32))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached)
(defextfun ("xrCreateSwapchainAndroidSurfaceKHR" create-swapchain-android-surface-khr 1) result
  (session session)
  (info (:pointer (:struct swapchain-create-info)))
  (swapchain (:pointer swapchain))
  (surface (:pointer j-object)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defcfun ("xrGetActionStateBoolean" get-action-state-boolean) result
  (session session)
  (get-info (:pointer (:struct action-state-get-info)))
  (state (:pointer (:struct action-state-boolean))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defcfun ("xrGetActionStateFloat" get-action-state-float) result
  (session session)
  (get-info (:pointer (:struct action-state-get-info)))
  (state (:pointer (:struct action-state-float))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defcfun ("xrGetActionStateVector2f" get-action-state-vector-2f) result
  (session session)
  (get-info (:pointer (:struct action-state-get-info)))
  (state (:pointer (:struct action-state-vector-2f))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defcfun ("xrGetActionStatePose" get-action-state-pose) result
  (session session)
  (get-info (:pointer (:struct action-state-get-info)))
  (state (:pointer (:struct action-state-pose))))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-out-of-memory
;;                 :error-limit-reached :error-path-format-invalid
;;                 :error-name-invalid :error-name-duplicated
;;                 :error-localized-name-invalid :error-localized-name-duplicated)
(defcfun ("xrCreateActionSet" create-action-set) result
  (instance instance)
  (create-info (:pointer (:struct action-set-create-info)))
  (action-set (:pointer action-set)))

;; success codes: (:success)
;;   error codes: (:error-handle-invalid)
(defcfun ("xrDestroyActionSet" destroy-action-set) result
  ;; externsync = "true_with_children"
  (action-set action-set))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-out-of-memory
;;                 :error-limit-reached :error-path-unsupported
;;                 :error-path-invalid :error-path-format-invalid
;;                 :error-name-invalid :error-name-duplicated
;;                 :error-localized-name-invalid :error-localized-name-duplicated
;;                 :error-actionsets-already-attached)
(defcfun ("xrCreateAction" create-action) result
  (action-set action-set)
  (create-info (:pointer (:struct action-create-info)))
  (action (:pointer action)))

;; success codes: (:success)
;;   error codes: (:error-handle-invalid)
(defcfun ("xrDestroyAction" destroy-action) result
  ;; externsync = "true_with_children"
  (action action))

;; success codes: (:success)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-actionsets-already-attached)
(defcfun ("xrSuggestInteractionProfileBindings" suggest-interaction-profile-bindings) result
  (instance instance)
  (suggested-bindings (:pointer (:struct interaction-profile-suggested-binding))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-actionsets-already-attached)
(defcfun ("xrAttachSessionActionSets" attach-session-action-sets) result
  (session session)
  (attach-info (:pointer (:struct session-action-sets-attach-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-actionset-not-attached)
(defcfun ("xrGetCurrentInteractionProfile" get-current-interaction-profile) result
  (session session)
  (top-level-user-path path)
  (interaction-profile (:pointer (:struct interaction-profile-state))))

;; success codes: (:success :session-loss-pending :session-not-focused)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-actionset-not-attached)
(defcfun ("xrSyncActions" sync-actions) result
  (session session)
  (sync-info (:pointer (:struct actions-sync-info))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-size-insufficient :error-path-invalid
;;                 :error-actionset-not-attached)
(defcfun ("xrEnumerateBoundSourcesForAction" enumerate-bound-sources-for-action) result
  (session session)
  (enumerate-info (:pointer (:struct bound-sources-for-action-enumerate-info)))
  ;; optional = "true"
  (source-capacity-input :uint32)
  (source-count-output (:pointer :uint32))
  ;; length = source-capacity-input
  ;; optional = "true"
  (sources (:pointer path)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-validation-failure :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-size-insufficient :error-path-unsupported
;;                 :error-path-invalid :error-actionset-not-attached)
(defcfun ("xrGetInputSourceLocalizedName" get-input-source-localized-name) result
  (session session)
  (get-info (:pointer (:struct input-source-localized-name-get-info)))
  ;; optional = "true"
  (buffer-capacity-input :uint32)
  (buffer-count-output (:pointer :uint32))
  ;; length = buffer-capacity-input
  ;; optional = "true"
  (buffer :string))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-size-insufficient
;;                 :error-system-invalid)
(defextfun ("xrGetVulkanInstanceExtensionsKHR" get-vulkan-instance-extensions-khr 2) result
  (instance instance)
  (system-id system-id)
  ;; optional = "true"
  (buffer-capacity-input :uint32)
  (buffer-count-output (:pointer :uint32))
  ;; length = buffer-capacity-input
  ;; optional = "true"
  (buffer :string))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-size-insufficient
;;                 :error-system-invalid)
(defextfun ("xrGetVulkanDeviceExtensionsKHR" get-vulkan-device-extensions-khr 3) result
  (instance instance)
  (system-id system-id)
  ;; optional = "true"
  (buffer-capacity-input :uint32)
  (buffer-count-output (:pointer :uint32))
  ;; length = buffer-capacity-input
  ;; optional = "true"
  (buffer :string))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetVulkanGraphicsDeviceKHR" get-vulkan-graphics-device-khr 4) result
  (instance instance)
  (system-id system-id)
  (vk-instance vk-image)
  (vk-physical-device (:pointer vk-physical-device)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetOpenGLGraphicsRequirementsKHR" get-opengl-graphics-requirements-khr 5) result
  (instance instance)
  (system-id system-id)
  (graphics-requirements (:pointer (:struct graphics-requirements-opengl-khr))))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetOpenGLESGraphicsRequirementsKHR" get-opengl-es-graphics-requirements-khr 6) result
  (instance instance)
  (system-id system-id)
  (graphics-requirements (:pointer
                          (:struct graphics-requirements-opengl-es-khr))))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetVulkanGraphicsRequirementsKHR" get-vulkan-graphics-requirements-khr 7) result
  (instance instance)
  (system-id system-id)
  (graphics-requirements (:pointer (:struct graphics-requirements-vulkan-khr))))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetD3D11GraphicsRequirementsKHR" get-d3d11-graphics-requirements-khr 8) result
  (instance instance)
  (system-id system-id)
  (graphics-requirements (:pointer (:struct graphics-requirements-d3d11-khr))))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetD3D12GraphicsRequirementsKHR" get-d3d12-graphics-requirements-khr 9) result
  (instance instance)
  (system-id system-id)
  (graphics-requirements (:pointer (:struct graphics-requirements-d3d12-khr))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrPerfSettingsSetPerformanceLevelEXT" perf-settings-set-performance-level-ext 10) result
  (session session)
  (domain perf-settings-domain-ext)
  (level perf-settings-level-ext))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrThermalGetTemperatureTrendEXT" thermal-get-temperature-trend-ext 11) result
  (session session)
  (domain perf-settings-domain-ext)
  (notification-level (:pointer perf-settings-notification-level-ext))
  (temp-headroom (:pointer :float))
  (temp-slope (:pointer :float)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-out-of-memory)
(defextfun ("xrSetDebugUtilsObjectNameEXT" set-debug-utils-object-name-ext 12) result
  (instance instance)
  ;; externsync = "nameInfo.objectHandle"
  (name-info (:pointer (:struct debug-utils-object-name-info-ext))))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-out-of-memory :error-limit-reached)
(defextfun ("xrCreateDebugUtilsMessengerEXT" create-debug-utils-messenger-ext 13) result
  ;; externsync = "true_with_children"
  (instance instance)
  (create-info (:pointer (:struct debug-utils-messenger-create-info-ext)))
  (messenger (:pointer debug-utils-messenger-ext)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroyDebugUtilsMessengerEXT" destroy-debug-utils-messenger-ext 14) result
  ;; Implicit external sync: the INSTANCE object used to create MESSENGER
  ;; externsync = "true"
  (messenger debug-utils-messenger-ext))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost)
(defextfun ("xrSubmitDebugUtilsMessageEXT" submit-debug-utils-message-ext 15) result
  (instance instance)
  (message-severity debug-utils-message-severity-flags-ext)
  (message-types debug-utils-message-type-flags-ext)
  (callback-data (:pointer (:struct debug-utils-messenger-callback-data-ext))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrSessionBeginDebugUtilsLabelRegionEXT" session-begin-debug-utils-label-region-ext 16) result
  (session session)
  (label-info (:pointer (:struct debug-utils-label-ext))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrSessionEndDebugUtilsLabelRegionEXT" session-end-debug-utils-label-region-ext 17) result
  (session session))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrSessionInsertDebugUtilsLabelEXT" session-insert-debug-utils-label-ext 18) result
  (session session)
  (label-info (:pointer (:struct debug-utils-label-ext))))

;; commands for XR_KHR_win32_convert_performance_counter_time
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-time-invalid)
(defextfun ("xrConvertTimeToWin32PerformanceCounterKHR" convert-time-to-win32-performance-counter-khr 19) result
  (instance instance)
  (time time)
  (performance-counter (:pointer large-integer)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-time-invalid)
(defextfun ("xrConvertWin32PerformanceCounterToTimeKHR" convert-win32-performance-counter-to-time-khr 20) result
  (instance instance)
  (performance-counter (:pointer large-integer))
  (time (:pointer time)))

;; commands for XR_KHR_vulkan_enable2
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-out-of-memory :error-limit-reached
;;                 :error-system-invalid)
(defextfun ("xrCreateVulkanInstanceKHR" create-vulkan-instance-khr 21) result
  (instance instance)
  (create-info (:pointer (:struct vulkan-instance-create-info-khr)))
  (vulkan-instance (:pointer vk-image))
  (vulkan-result (:pointer vk-result)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-out-of-memory :error-limit-reached
;;                 :error-system-invalid)
(defextfun ("xrCreateVulkanDeviceKHR" create-vulkan-device-khr 22) result
  (instance instance)
  (create-info (:pointer (:struct vulkan-device-create-info-khr)))
  (vulkan-device (:pointer vk-device))
  (vulkan-result (:pointer vk-result)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetVulkanGraphicsDevice2KHR" get-vulkan-graphics-device-2-khr 23) result
  (instance instance)
  (get-info (:pointer (:struct vulkan-graphics-device-get-info-khr)))
  (vulkan-physical-device (:pointer vk-physical-device)))

;; alias function GET-VULKAN-GRAPHICS-REQUIREMENTS-2-KHR -> GET-VULKAN-GRAPHICS-REQUIREMENTS-KHR
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-system-invalid)
(defextfun ("xrGetVulkanGraphicsRequirements2KHR" get-vulkan-graphics-requirements-2-khr 24) result
  (instance instance)
  (system-id system-id)
  (graphics-requirements (:pointer (:struct graphics-requirements-vulkan-khr))))

;; commands for XR_KHR_convert_timespec_time
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-time-invalid)
(defextfun ("xrConvertTimeToTimespecTimeKHR" convert-time-to-timespec-time-khr 25) result
  (instance instance)
  (time time)
  (timespec-time (:pointer timespec)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-time-invalid)
(defextfun ("xrConvertTimespecTimeToTimeKHR" convert-timespec-time-to-time-khr 26) result
  (instance instance)
  (timespec-time (:pointer timespec))
  (time (:pointer time)))

;; commands for XR_KHR_visibility_mask
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-size-insufficient
;;                 :error-view-configuration-type-unsupported)
(defextfun ("xrGetVisibilityMaskKHR" get-visibility-mask-khr 27) result
  (session session)
  (view-configuration-type view-configuration-type)
  (view-index :uint32)
  (visibility-mask-type visibility-mask-type-khr)
  (visibility-mask (:pointer (:struct visibility-mask-khr))))

;; commands for XR_MSFT_spatial_anchor
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached :error-time-invalid
;;                 :error-pose-invalid :error-create-spatial-anchor-failed-msft)
(defextfun ("xrCreateSpatialAnchorMSFT" create-spatial-anchor-msft 28) result
  (session session)
  (create-info (:pointer (:struct spatial-anchor-create-info-msft)))
  (anchor (:pointer spatial-anchor-msft)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached :error-pose-invalid)
(defextfun ("xrCreateSpatialAnchorSpaceMSFT" create-spatial-anchor-space-msft 29) result
  (session session)
  (create-info (:pointer (:struct spatial-anchor-space-create-info-msft)))
  (space (:pointer space)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroySpatialAnchorMSFT" destroy-spatial-anchor-msft 30) result
  ;; externsync = "true_with_children"
  (anchor spatial-anchor-msft))

;; commands for XR_EXT_conformance_automation
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid)
(defextfun ("xrSetInputDeviceActiveEXT" set-input-device-active-ext 31) result
  (session session)
  (interaction-profile path)
  (top-level-path path)
  (is-active bool-32))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid)
(defextfun ("xrSetInputDeviceStateBoolEXT" set-input-device-state-bool-ext 32) result
  (session session)
  (top-level-path path)
  (input-source-path path)
  (state bool-32))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid)
(defextfun ("xrSetInputDeviceStateFloatEXT" set-input-device-state-float-ext 33) result
  (session session)
  (top-level-path path)
  (input-source-path path)
  (state :float))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid)
#++
(defextfun ("xrSetInputDeviceStateVector2fEXT" set-input-device-state-vector-2f-ext 34) result
  (session session)
  (top-level-path path)
  (input-source-path path)
  (state (:struct vector-2f)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-pose-invalid :error-path-unsupported :error-path-invalid)
#++
(defextfun ("xrSetInputDeviceLocationEXT" set-input-device-location-ext 35) result
  (session session)
  (top-level-path path)
  (input-source-path path)
  (space space)
  (pose (:struct pose-f)))

;; commands for XR_KHR_loader_init
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure)
(defextfun ("xrInitializeLoaderKHR" initialize-loader-khr 36) result
  (loader-init-info (:pointer (:struct loader-init-info-base-header-khr))))

;; commands for XR_MSFT_spatial_graph_bridge
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached :error-pose-invalid)
(defextfun ("xrCreateSpatialGraphNodeSpaceMSFT" create-spatial-graph-node-space-msft 37) result
  (session session)
  (create-info (:pointer (:struct spatial-graph-node-space-create-info-msft)))
  (space (:pointer space)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-time-invalid :error-pose-invalid)
(defextfun ("xrTryCreateSpatialGraphStaticNodeBindingMSFT" try-create-spatial-graph-static-node-binding-msft 38) result
  (session session)
  (create-info (:pointer
                (:struct spatial-graph-static-node-binding-create-info-msft)))
  (node-binding (:pointer spatial-graph-node-binding-msft)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroySpatialGraphNodeBindingMSFT" destroy-spatial-graph-node-binding-msft 39) result
  ;; externsync = "true_with_children"
  (node-binding spatial-graph-node-binding-msft))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory)
(defextfun ("xrGetSpatialGraphNodeBindingPropertiesMSFT" get-spatial-graph-node-binding-properties-msft 40) result
  (node-binding spatial-graph-node-binding-msft)
  ;; optional = "true"
  (get-info (:pointer
             (:struct spatial-graph-node-binding-properties-get-info-msft)))
  (properties (:pointer (:struct spatial-graph-node-binding-properties-msft))))

;; XR_EXT_hand_tracking
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreateHandTrackerEXT" create-hand-tracker-ext 41) result
  (session session)
  (create-info (:pointer (:struct hand-tracker-create-info-ext)))
  (hand-tracker (:pointer hand-tracker-ext)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroyHandTrackerEXT" destroy-hand-tracker-ext 42) result
  ;; externsync = "true_with_children"
  (hand-tracker hand-tracker-ext))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-time-invalid)
(defextfun ("xrLocateHandJointsEXT" locate-hand-joints-ext 43) result
  (hand-tracker hand-tracker-ext)
  (locate-info (:pointer (:struct hand-joints-locate-info-ext)))
  (locations (:pointer (:struct hand-joint-locations-ext))))

;; XR_FB_face_tracking
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreateFaceTrackerFB" create-face-tracker-fb 44) result
  (session session)
  (create-info (:pointer (:struct face-tracker-create-info-fb)))
  (face-tracker (:pointer face-tracker-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroyFaceTrackerFB" destroy-face-tracker-fb 45) result
  ;; externsync = "true_with_children"
  (face-tracker face-tracker-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-time-invalid)
(defextfun ("xrGetFaceExpressionWeightsFB" get-face-expression-weights-fb 46) result
  (face-tracker face-tracker-fb)
  (expression-info (:pointer (:struct face-expression-info-fb)))
  (expression-weights (:pointer (:struct face-expression-weights-fb))))

;; XR_FB_body_tracking
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreateBodyTrackerFB" create-body-tracker-fb 47) result
  (session session)
  (create-info (:pointer (:struct body-tracker-create-info-fb)))
  (body-tracker (:pointer body-tracker-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroyBodyTrackerFB" destroy-body-tracker-fb 48) result
  ;; externsync = "true_with_children"
  (body-tracker body-tracker-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-time-invalid)
(defextfun ("xrLocateBodyJointsFB" locate-body-joints-fb 49) result
  (body-tracker body-tracker-fb)
  (locate-info (:pointer (:struct body-joints-locate-info-fb)))
  (locations (:pointer (:struct body-joint-locations-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrGetBodySkeletonFB" get-body-skeleton-fb 50) result
  (body-tracker body-tracker-fb)
  (skeleton (:pointer (:struct body-skeleton-fb))))

;; XR_FB_eye_tracking_social
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreateEyeTrackerFB" create-eye-tracker-fb 51) result
  (session session)
  (create-info (:pointer (:struct eye-tracker-create-info-fb)))
  (eye-tracker (:pointer eye-tracker-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroyEyeTrackerFB" destroy-eye-tracker-fb 52) result
  ;; externsync = "true_with_children"
  (eye-tracker eye-tracker-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-time-invalid)
(defextfun ("xrGetEyeGazesFB" get-eye-gazes-fb 53) result
  (eye-tracker eye-tracker-fb)
  (gaze-info (:pointer (:struct eye-gazes-info-fb)))
  (eye-gazes (:pointer (:struct eye-gazes-fb))))

;; XR_MSFT_hand_tracking_mesh
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-pose-invalid
;;                 :error-feature-unsupported)
(defextfun ("xrCreateHandMeshSpaceMSFT" create-hand-mesh-space-msft 54) result
  (hand-tracker hand-tracker-ext)
  (create-info (:pointer (:struct hand-mesh-space-create-info-msft)))
  (space (:pointer space)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-size-insufficient :error-time-invalid
;;                 :error-feature-unsupported)
(defextfun ("xrUpdateHandMeshMSFT" update-hand-mesh-msft 55) result
  (hand-tracker hand-tracker-ext)
  (update-info (:pointer (:struct hand-mesh-update-info-msft)))
  (hand-mesh (:pointer (:struct hand-mesh-msft))))

;; XR_MSFT_controller_model
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-path-unsupported
;;                 :error-path-invalid :error-controller-model-key-invalid-msft)
(defextfun ("xrGetControllerModelKeyMSFT" get-controller-model-key-msft 56) result
  (session session)
  (top-level-user-path path)
  (controller-model-key-state (:pointer
                               (:struct controller-model-key-state-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-size-insufficient
;;                 :error-controller-model-key-invalid-msft)
(defextfun ("xrLoadControllerModelMSFT" load-controller-model-msft 57) result
  (session session)
  (model-key controller-model-key-msft)
  ;; optional = "true"
  (buffer-capacity-input :uint32)
  (buffer-count-output (:pointer :uint32))
  ;; length = buffer-capacity-input
  ;; optional = "true"
  (buffer (:pointer :uint8)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-controller-model-key-invalid-msft)
(defextfun ("xrGetControllerModelPropertiesMSFT" get-controller-model-properties-msft 58) result
  (session session)
  (model-key controller-model-key-msft)
  (properties (:pointer (:struct controller-model-properties-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-controller-model-key-invalid-msft)
(defextfun ("xrGetControllerModelStateMSFT" get-controller-model-state-msft 59) result
  (session session)
  (model-key controller-model-key-msft)
  (state (:pointer (:struct controller-model-state-msft))))

;; XR_MSFT_scene_understanding
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-out-of-memory
;;                 :error-size-insufficient :error-system-invalid)
(defextfun ("xrEnumerateSceneComputeFeaturesMSFT" enumerate-scene-compute-features-msft 60) result
  (instance instance)
  (system-id system-id)
  ;; optional = "true"
  (feature-capacity-input :uint32)
  (feature-count-output (:pointer :uint32))
  ;; length = feature-capacity-input
  ;; optional = "true"
  (features (:pointer scene-compute-feature-msft)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached)
(defextfun ("xrCreateSceneObserverMSFT" create-scene-observer-msft 61) result
  (session session)
  ;; optional = "true"
  (create-info (:pointer (:struct scene-observer-create-info-msft)))
  (scene-observer (:pointer scene-observer-msft)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroySceneObserverMSFT" destroy-scene-observer-msft 62) result
  ;; externsync = "true_with_children"
  (scene-observer scene-observer-msft))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached
;;                 :error-compute-new-scene-not-completed-msft)
(defextfun ("xrCreateSceneMSFT" create-scene-msft 63) result
  (scene-observer scene-observer-msft)
  ;; optional = "true"
  (create-info (:pointer (:struct scene-create-info-msft)))
  (scene (:pointer scene-msft)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroySceneMSFT" destroy-scene-msft 64) result
  ;; externsync = "true_with_children"
  (scene scene-msft))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-time-invalid
;;                 :error-scene-compute-feature-incompatible-msft
;;                 :error-scene-compute-consistency-mismatch-msft
;;                 :error-pose-invalid :error-compute-new-scene-not-completed-msft)
(defextfun ("xrComputeNewSceneMSFT" compute-new-scene-msft 65) result
  (scene-observer scene-observer-msft)
  (compute-info (:pointer (:struct new-scene-compute-info-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory)
(defextfun ("xrGetSceneComputeStateMSFT" get-scene-compute-state-msft 66) result
  (scene-observer scene-observer-msft)
  (state (:pointer scene-compute-state-msft)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-size-insufficient
;;                 :error-scene-component-type-mismatch-msft)
(defextfun ("xrGetSceneComponentsMSFT" get-scene-components-msft 67) result
  (scene scene-msft)
  (get-info (:pointer (:struct scene-components-get-info-msft)))
  (components (:pointer (:struct scene-components-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-size-insufficient :error-time-invalid)
(defextfun ("xrLocateSceneComponentsMSFT" locate-scene-components-msft 68) result
  (scene scene-msft)
  (locate-info (:pointer (:struct scene-components-locate-info-msft)))
  (locations (:pointer (:struct scene-component-locations-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-scene-mesh-buffer-id-invalid-msft
;;                 :error-scene-component-id-invalid-msft)
(defextfun ("xrGetSceneMeshBuffersMSFT" get-scene-mesh-buffers-msft 69) result
  (scene scene-msft)
  (get-info (:pointer (:struct scene-mesh-buffers-get-info-msft)))
  (buffers (:pointer (:struct scene-mesh-buffers-msft))))

;; XR_MSFT_scene_understanding_serialization
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-compute-new-scene-not-completed-msft)
(defextfun ("xrDeserializeSceneMSFT" deserialize-scene-msft 70) result
  (scene-observer scene-observer-msft)
  (deserialize-info (:pointer (:struct scene-deserialize-info-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-size-insufficient :error-scene-component-id-invalid-msft)
(defextfun ("xrGetSerializedSceneFragmentDataMSFT" get-serialized-scene-fragment-data-msft 71) result
  (scene scene-msft)
  (get-info (:pointer (:struct serialized-scene-fragment-data-get-info-msft)))
  ;; optional = "true"
  (count-input :uint32)
  (read-output (:pointer :uint32))
  ;; length = count-input
  ;; optional = "true"
  (buffer (:pointer :uint8)))

;; XR_FB_display_refresh_rate
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-size-insufficient)
(defextfun ("xrEnumerateDisplayRefreshRatesFB" enumerate-display-refresh-rates-fb 72) result
  (session session)
  ;; optional = "true"
  (display-refresh-rate-capacity-input :uint32)
  (display-refresh-rate-count-output (:pointer :uint32))
  ;; length = display-refresh-rate-capacity-input
  ;; optional = "true"
  (display-refresh-rates (:pointer :float)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrGetDisplayRefreshRateFB" get-display-refresh-rate-fb 73) result
  (session session)
  (display-refresh-rate (:pointer :float)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported
;;                 :error-display-refresh-rate-unsupported-fb)
(defextfun ("xrRequestDisplayRefreshRateFB" request-display-refresh-rate-fb 74) result
  (session session)
  (display-refresh-rate :float))

;; XR_MSFT_perception_anchor_interop
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached)
(defextfun ("xrCreateSpatialAnchorFromPerceptionAnchorMSFT" create-spatial-anchor-from-perception-anchor-msft 75) result
  (session session)
  (perception-anchor (:pointer i-unknown))
  (anchor (:pointer spatial-anchor-msft)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory)
(defextfun ("xrTryGetPerceptionAnchorFromSpatialAnchorMSFT" try-get-perception-anchor-from-spatial-anchor-msft 76) result
  (session session)
  (anchor spatial-anchor-msft)
  (perception-anchor (:pointer (:pointer i-unknown))))

;; XR_FB_swapchain_update_state
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrUpdateSwapchainFB" update-swapchain-fb 77) result
  (swapchain swapchain)
  (state (:pointer (:struct swapchain-state-base-header-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrGetSwapchainStateFB" get-swapchain-state-fb 78) result
  (swapchain swapchain)
  (state (:pointer (:struct swapchain-state-base-header-fb))))

;; XR_FB_color_space
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-size-insufficient)
(defextfun ("xrEnumerateColorSpacesFB" enumerate-color-spaces-fb 79) result
  (session session)
  ;; optional = "true"
  (color-space-capacity-input :uint32)
  (color-space-count-output (:pointer :uint32))
  ;; length = color-space-capacity-input
  ;; optional = "true"
  (color-spaces (:pointer color-space-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported :error-color-space-unsupported-fb)
(defextfun ("xrSetColorSpaceFB" set-color-space-fb 80) result
  (session session)
  (colorspace color-space-fb))

;; XR_FB_foveation
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached)
(defextfun ("xrCreateFoveationProfileFB" create-foveation-profile-fb 81) result
  (session session)
  (create-info (:pointer (:struct foveation-profile-create-info-fb)))
  (profile (:pointer foveation-profile-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid)
(defextfun ("xrDestroyFoveationProfileFB" destroy-foveation-profile-fb 82) result
  ;; externsync = "true_with_children"
  (profile foveation-profile-fb))

;; XR_META_foveation_eye_tracked
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrGetFoveationEyeTrackedStateMETA" get-foveation-eye-tracked-state-meta 83) result
  (session session)
  (foveation-state (:pointer (:struct foveation-eye-tracked-state-meta))))

;; XR_FB_hand_tracking_mesh
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-size-insufficient :error-feature-unsupported)
(defextfun ("xrGetHandMeshFB" get-hand-mesh-fb 84) result
  (hand-tracker hand-tracker-ext)
  (mesh (:pointer (:struct hand-tracking-mesh-fb))))

;; XR_FB_render_model
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory)
(defextfun ("xrEnumerateRenderModelPathsFB" enumerate-render-model-paths-fb 85) result
  (session session)
  ;; optional = "true"
  (path-capacity-input :uint32)
  (path-count-output (:pointer :uint32))
  ;; length = path-capacity-input
  ;; optional = "true"
  (paths (:pointer (:struct render-model-path-info-fb))))

;; success codes: (:success :session-loss-pending :nder-model-unavailable-fb)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-path-unsupported
;;                 :error-path-invalid :error-call-order-invalid)
(defextfun ("xrGetRenderModelPropertiesFB" get-render-model-properties-fb 86) result
  (session session)
  (path path)
  (properties (:pointer (:struct render-model-properties-fb))))

;; success codes: (:success :session-loss-pending :nder-model-unavailable-fb)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-render-model-key-invalid-fb)
(defextfun ("xrLoadRenderModelFB" load-render-model-fb 87) result
  (session session)
  (info (:pointer (:struct render-model-load-info-fb)))
  (buffer (:pointer (:struct render-model-buffer-fb))))

;; XR_FB_keyboard_tracking
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrQuerySystemTrackedKeyboardFB" query-system-tracked-keyboard-fb 88) result
  (session session)
  (query-info (:pointer (:struct keyboard-tracking-query-fb)))
  (keyboard (:pointer (:struct keyboard-tracking-description-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreateKeyboardSpaceFB" create-keyboard-space-fb 89) result
  (session session)
  (create-info (:pointer (:struct keyboard-space-create-info-fb)))
  (keyboard-space (:pointer space)))

;; commands for XR_VARJO_environment_depth_estimation
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrSetEnvironmentDepthEstimationVARJO" set-environment-depth-estimation-varjo 90) result
  (session session)
  (enabled bool-32))

;; XR_MSFT_composition_layer_reprojection
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-size-insufficient
;;                 :error-view-configuration-type-unsupported
;;                 :error-system-invalid)
(defextfun ("xrEnumerateReprojectionModesMSFT" enumerate-reprojection-modes-msft 91) result
  (instance instance)
  (system-id system-id)
  (view-configuration-type view-configuration-type)
  ;; optional = "true"
  (mode-capacity-input :uint32)
  (mode-count-output (:pointer :uint32))
  ;; length = mode-capacity-input
  ;; optional = "true"
  (modes (:pointer reprojection-mode-msft)))

;; commands for XR_OCULUS_audio_device_guid
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-feature-unsupported)
(defextfun ("xrGetAudioOutputDeviceGuidOculus" get-audio-output-device-guid-oculus 92) result
  (instance instance)
  ;; length = +MAX-AUDIO-DEVICE-STR-SIZE-OCULUS+ (128)
  (buffer (:pointer :uint16)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-feature-unsupported)
(defextfun ("xrGetAudioInputDeviceGuidOculus" get-audio-input-device-guid-oculus 93) result
  (instance instance)
  ;; length = +MAX-AUDIO-DEVICE-STR-SIZE-OCULUS+ (128)
  (buffer (:pointer :uint16)))

;; XR_FB_spatial_entity
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-time-invalid :error-pose-invalid
;;                 :error-feature-unsupported)
(defextfun ("xrCreateSpatialAnchorFB" create-spatial-anchor-fb 94) result
  (session session)
  (info (:pointer (:struct spatial-anchor-create-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceUuidFB" get-space-uuid-fb 95) result
  (space space)
  (uuid (:pointer (:struct uuid-ext))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrEnumerateSpaceSupportedComponentsFB" enumerate-space-supported-components-fb 96) result
  (space space)
  ;; optional = "true"
  (component-type-capacity-input :uint32)
  (component-type-count-output (:pointer :uint32))
  ;; length = component-type-capacity-input
  ;; optional = "true"
  (component-types (:pointer space-component-type-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-status-pending-fb
;;                 :error-space-component-status-already-set-fb
;;                 :error-space-component-not-supported-fb
;;                 :error-feature-unsupported)
(defextfun ("xrSetSpaceComponentStatusFB" set-space-component-status-fb 97) result
  (space space)
  (info (:pointer (:struct space-component-status-set-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-supported-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceComponentStatusFB" get-space-component-status-fb 98) result
  (space space)
  (component-type space-component-type-fb)
  (status (:pointer (:struct space-component-status-fb))))

;; XR_FB_triangle_mesh
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached
;;                 :error-insufficient-resources-passthrough-fb
;;                 :error-feature-unsupported)
(defextfun ("xrCreateTriangleMeshFB" create-triangle-mesh-fb 99) result
  (session session)
  (create-info (:pointer (:struct triangle-mesh-create-info-fb)))
  (out-triangle-mesh (:pointer triangle-mesh-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-feature-unsupported)
(defextfun ("xrDestroyTriangleMeshFB" destroy-triangle-mesh-fb 100) result
  ;; Implicit external sync: the buffers returned from calls to TRIANGLE-MESH-GET-VERTEX-BUFFER-FB call and TRIANGLE-MESH-GET-INDEX-BUFFER-FB call on MESH
  ;; externsync = "true_with_children"
  (mesh triangle-mesh-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrTriangleMeshGetVertexBufferFB" triangle-mesh-get-vertex-buffer-fb 101) result
  (mesh triangle-mesh-fb)
  (out-vertex-buffer (:pointer (:pointer (:struct vector-3f)))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrTriangleMeshGetIndexBufferFB" triangle-mesh-get-index-buffer-fb 102) result
  (mesh triangle-mesh-fb)
  (out-index-buffer (:pointer (:pointer :uint32))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported :error-call-order-invalid)
(defextfun ("xrTriangleMeshBeginUpdateFB" triangle-mesh-begin-update-fb 103) result
  (mesh triangle-mesh-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported :error-call-order-invalid)
(defextfun ("xrTriangleMeshEndUpdateFB" triangle-mesh-end-update-fb 104) result
  (mesh triangle-mesh-fb)
  (vertex-count :uint32)
  (triangle-count :uint32))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported :error-call-order-invalid)
(defextfun ("xrTriangleMeshBeginVertexBufferUpdateFB" triangle-mesh-begin-vertex-buffer-update-fb 105) result
  (mesh triangle-mesh-fb)
  (out-vertex-count (:pointer :uint32)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported :error-call-order-invalid)
(defextfun ("xrTriangleMeshEndVertexBufferUpdateFB" triangle-mesh-end-vertex-buffer-update-fb 106) result
  (mesh triangle-mesh-fb))

;; XR_FB_passthrough
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-unknown-passthrough-fb
;;                 :error-not-permitted-passthrough-fb :error-feature-unsupported
;;                 :error-feature-already-created-passthrough-fb)
(defextfun ("xrCreatePassthroughFB" create-passthrough-fb 107) result
  (session session)
  (create-info (:pointer (:struct passthrough-create-info-fb)))
  (out-passthrough (:pointer passthrough-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-feature-unsupported)
(defextfun ("xrDestroyPassthroughFB" destroy-passthrough-fb 108) result
  ;; externsync = "true_with_children"
  (passthrough passthrough-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-unexpected-state-passthrough-fb
;;                 :error-feature-unsupported)
(defextfun ("xrPassthroughStartFB" passthrough-start-fb 109) result
  (passthrough passthrough-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-unexpected-state-passthrough-fb
;;                 :error-feature-unsupported)
(defextfun ("xrPassthroughPauseFB" passthrough-pause-fb 110) result
  (passthrough passthrough-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-unknown-passthrough-fb
;;                 :error-insufficient-resources-passthrough-fb
;;                 :error-feature-unsupported
;;                 :error-feature-required-passthrough-fb)
(defextfun ("xrCreatePassthroughLayerFB" create-passthrough-layer-fb 111) result
  (session session)
  (create-info (:pointer (:struct passthrough-layer-create-info-fb)))
  (out-layer (:pointer passthrough-layer-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-feature-unsupported)
(defextfun ("xrDestroyPassthroughLayerFB" destroy-passthrough-layer-fb 112) result
  ;; externsync = "true_with_children"
  (layer passthrough-layer-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-unexpected-state-passthrough-fb
;;                 :error-feature-unsupported)
(defextfun ("xrPassthroughLayerPauseFB" passthrough-layer-pause-fb 113) result
  (layer passthrough-layer-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-unexpected-state-passthrough-fb
;;                 :error-feature-unsupported)
(defextfun ("xrPassthroughLayerResumeFB" passthrough-layer-resume-fb 114) result
  (layer passthrough-layer-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrPassthroughLayerSetStyleFB" passthrough-layer-set-style-fb 115) result
  (layer passthrough-layer-fb)
  (style (:pointer (:struct passthrough-style-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-pose-invalid
;;                 :error-insufficient-resources-passthrough-fb
;;                 :error-feature-unsupported)
(defextfun ("xrCreateGeometryInstanceFB" create-geometry-instance-fb 116) result
  (session session)
  (create-info (:pointer (:struct geometry-instance-create-info-fb)))
  (out-geometry-instance (:pointer geometry-instance-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-feature-unsupported)
(defextfun ("xrDestroyGeometryInstanceFB" destroy-geometry-instance-fb 117) result
  ;; externsync = "true_with_children"
  (instance geometry-instance-fb))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-time-invalid
;;                 :error-pose-invalid :error-feature-unsupported)
(defextfun ("xrGeometryInstanceSetTransformFB" geometry-instance-set-transform-fb 118) result
  (instance geometry-instance-fb)
  (transformation (:pointer (:struct geometry-instance-transform-fb))))

;; XR_FB_spatial_entity_query
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrQuerySpacesFB" query-spaces-fb 119) result
  (session session)
  (info (:pointer (:struct space-query-info-base-header-fb)))
  (request-id (:pointer async-request-id-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrRetrieveSpaceQueryResultsFB" retrieve-space-query-results-fb 120) result
  (session session)
  (request-id async-request-id-fb)
  (results (:pointer (:struct space-query-results-fb))))

;; XR_FB_spatial_entity_storage
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrSaveSpaceFB" save-space-fb 121) result
  (session session)
  (info (:pointer (:struct space-save-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrEraseSpaceFB" erase-space-fb 122) result
  (session session)
  (info (:pointer (:struct space-erase-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; XR_FB_spatial_entity_storage_batch
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-network-timeout-fb
;;                 :error-space-network-request-failed-fb
;;                 :error-space-mapping-insufficient-fb
;;                 :error-space-localization-failed-fb
;;                 :error-space-component-not-enabled-fb
;;                 :error-space-cloud-storage-disabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrSaveSpaceListFB" save-space-list-fb 123) result
  (session session)
  (info (:pointer (:struct space-list-save-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; XR_FB_spatial_entity_sharing
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-network-timeout-fb
;;                 :error-space-network-request-failed-fb
;;                 :error-space-mapping-insufficient-fb
;;                 :error-space-localization-failed-fb
;;                 :error-space-component-not-enabled-fb
;;                 :error-space-cloud-storage-disabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrShareSpacesFB" share-spaces-fb 124) result
  (session session)
  (info (:pointer (:struct space-share-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; XR_FB_spatial_entity_container
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceContainerFB" get-space-container-fb 125) result
  (session session)
  (space space)
  (space-container-output (:pointer (:struct space-container-fb))))

;; XR_FB_scene
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceBoundingBox2DFB" get-space-bounding-box-2d-fb 126) result
  (session session)
  (space space)
  (bounding-box-2d-output (:pointer (:struct rect-2d-f))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceBoundingBox3DFB" get-space-bounding-box-3d-fb 127) result
  (session session)
  (space space)
  (bounding-box-3d-output (:pointer (:struct rect-3d-f-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceSemanticLabelsFB" get-space-semantic-labels-fb 128) result
  (session session)
  (space space)
  (semantic-labels-output (:pointer (:struct semantic-labels-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceBoundary2DFB" get-space-boundary-2d-fb 129) result
  (session session)
  (space space)
  (boundary-2d-output (:pointer (:struct boundary-2d-fb))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-space-component-not-enabled-fb
;;                 :error-feature-unsupported)
(defextfun ("xrGetSpaceRoomLayoutFB" get-space-room-layout-fb 130) result
  (session session)
  (space space)
  (room-layout-output (:pointer (:struct room-layout-fb))))

;; XR_FB_scene_capture
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrRequestSceneCaptureFB" request-scene-capture-fb 131) result
  (session session)
  (info (:pointer (:struct scene-capture-request-info-fb)))
  (request-id (:pointer async-request-id-fb)))

;; XR_FB_passthrough_keyboard_hands
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrPassthroughLayerSetKeyboardHandsIntensityFB" passthrough-layer-set-keyboard-hands-intensity-fb 132) result
  (layer passthrough-layer-fb)
  (intensity (:pointer (:struct passthrough-keyboard-hands-intensity-fb))))

;; XR_MSFT_spatial_anchor_persistence
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached)
(defextfun ("xrCreateSpatialAnchorStoreConnectionMSFT" create-spatial-anchor-store-connection-msft 133) result
  (session session)
  (spatial-anchor-store (:pointer spatial-anchor-store-connection-msft)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid
;;                 :error-out-of-memory)
(defextfun ("xrDestroySpatialAnchorStoreConnectionMSFT" destroy-spatial-anchor-store-connection-msft 134) result
  ;; externsync = "true_with_children"
  (spatial-anchor-store spatial-anchor-store-connection-msft))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-spatial-anchor-name-invalid-msft)
(defextfun ("xrPersistSpatialAnchorMSFT" persist-spatial-anchor-msft 135) result
  (spatial-anchor-store spatial-anchor-store-connection-msft)
  (spatial-anchor-persistence-info (:pointer
                                    (:struct
                                     spatial-anchor-persistence-info-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-size-insufficient)
(defextfun ("xrEnumeratePersistedSpatialAnchorNamesMSFT" enumerate-persisted-spatial-anchor-names-msft 136) result
  (spatial-anchor-store spatial-anchor-store-connection-msft)
  ;; optional = "true"
  (spatial-anchor-names-capacity-input :uint32)
  ;; optional = "true"
  (spatial-anchor-names-count-output (:pointer :uint32))
  ;; length = spatial-anchor-names-capacity-input
  ;; optional = "true"
  (persisted-anchor-names (:pointer
                           (:struct spatial-anchor-persistence-name-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached
;;                 :error-spatial-anchor-name-not-found-msft
;;                 :error-spatial-anchor-name-invalid-msft)
(defextfun ("xrCreateSpatialAnchorFromPersistedNameMSFT" create-spatial-anchor-from-persisted-name-msft 137) result
  (session session)
  (spatial-anchor-create-info (:pointer
                               (:struct
                                spatial-anchor-from-persisted-anchor-create-info-msft)))
  (spatial-anchor (:pointer spatial-anchor-msft)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-spatial-anchor-name-not-found-msft
;;                 :error-spatial-anchor-name-invalid-msft)
(defextfun ("xrUnpersistSpatialAnchorMSFT" unpersist-spatial-anchor-msft 138) result
  (spatial-anchor-store spatial-anchor-store-connection-msft)
  (spatial-anchor-persistence-name (:pointer
                                    (:struct
                                     spatial-anchor-persistence-name-msft))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory)
(defextfun ("xrClearSpatialAnchorStoreMSFT" clear-spatial-anchor-store-msft 139) result
  (spatial-anchor-store spatial-anchor-store-connection-msft))

;; XR_HTC_facial_tracking
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreateFacialTrackerHTC" create-facial-tracker-htc 140) result
  (session session)
  (create-info (:pointer (:struct facial-tracker-create-info-htc)))
  (facial-tracker (:pointer facial-tracker-htc)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-handle-invalid)
(defextfun ("xrDestroyFacialTrackerHTC" destroy-facial-tracker-htc 141) result
  ;; externsync = "true_with_children"
  (facial-tracker facial-tracker-htc))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-time-invalid)
(defextfun ("xrGetFacialExpressionsHTC" get-facial-expressions-htc 142) result
  (facial-tracker facial-tracker-htc)
  (facial-expressions (:pointer (:struct facial-expressions-htc))))

;; XR_HTC_passthrough
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-feature-unsupported)
(defextfun ("xrCreatePassthroughHTC" create-passthrough-htc 143) result
  (session session)
  (create-info (:pointer (:struct passthrough-create-info-htc)))
  (passthrough (:pointer passthrough-htc)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid)
(defextfun ("xrDestroyPassthroughHTC" destroy-passthrough-htc 144) result
  ;; externsync = "true_with_children"
  (passthrough passthrough-htc))

;; XR_HTCX_vive_tracker_interaction
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-size-insufficient)
(defextfun ("xrEnumerateViveTrackerPathsHTCX" enumerate-vive-tracker-paths-htcx 145) result
  (instance instance)
  ;; optional = "true"
  (path-capacity-input :uint32)
  (path-count-output (:pointer :uint32))
  ;; length = path-capacity-input
  ;; optional = "true"
  (paths (:pointer (:struct vive-tracker-paths-htcx))))

;; commands for XR_VARJO_marker_tracking
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrSetMarkerTrackingVARJO" set-marker-tracking-varjo 146) result
  (session session)
  (enabled bool-32))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-marker-id-invalid-varjo :error-feature-unsupported)
(defextfun ("xrSetMarkerTrackingTimeoutVARJO" set-marker-tracking-timeout-varjo 147) result
  (session session)
  (marker-id :uint64)
  (timeout duration))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-marker-id-invalid-varjo :error-feature-unsupported)
(defextfun ("xrSetMarkerTrackingPredictionVARJO" set-marker-tracking-prediction-varjo 148) result
  (session session)
  (marker-id :uint64)
  (enabled bool-32))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-marker-not-tracked-varjo :error-marker-id-invalid-varjo
;;                 :error-feature-unsupported)
(defextfun ("xrGetMarkerSizeVARJO" get-marker-size-varjo 149) result
  (session session)
  (marker-id :uint64)
  (size (:pointer (:struct extent-2d-f))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-out-of-memory :error-limit-reached :error-pose-invalid
;;                 :error-marker-id-invalid-varjo :error-feature-unsupported)
(defextfun ("xrCreateMarkerSpaceVARJO" create-marker-space-varjo 150) result
  (session session)
  (create-info (:pointer (:struct marker-space-create-info-varjo)))
  (space (:pointer space)))

;; commands for XR_ALMALENCE_digital_lens_control
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrSetDigitalLensControlALMALENCE" set-digital-lens-control-almalence 151) result
  (session session)
  (digital-lens-control (:pointer (:struct digital-lens-control-almalence))))

;; commands for XR_VARJO_view_offset
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-feature-unsupported)
(defextfun ("xrSetViewOffsetVARJO" set-view-offset-varjo 152) result
  (session session)
  (offset :float))

;; commands for XR_OCULUS_external_camera
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-size-insufficient)
(defextfun ("xrEnumerateExternalCamerasOCULUS" enumerate-external-cameras-oculus 153) result
  (session session)
  ;; optional = "true"
  (camera-capacity-input :uint32)
  (camera-count-output (:pointer :uint32))
  ;; length = camera-capacity-input
  ;; optional = "true"
  (cameras (:pointer (:struct external-camera-oculus))))

;; commands for XR_META_performance_metrics
;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-size-insufficient)
(defextfun ("xrEnumeratePerformanceMetricsCounterPathsMETA" enumerate-performance-metrics-counter-paths-meta 154) result
  (instance instance)
  ;; optional = "true"
  (counter-path-capacity-input :uint32)
  (counter-path-count-output (:pointer :uint32))
  ;; length = counter-path-capacity-input
  ;; optional = "true"
  (counter-paths (:pointer path)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrSetPerformanceMetricsStateMETA" set-performance-metrics-state-meta 155) result
  (session session)
  (state (:pointer (:struct performance-metrics-state-meta))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost)
(defextfun ("xrGetPerformanceMetricsStateMETA" get-performance-metrics-state-meta 156) result
  (session session)
  (state (:pointer (:struct performance-metrics-state-meta))))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid)
(defextfun ("xrQueryPerformanceMetricsCounterMETA" query-performance-metrics-counter-meta 157) result
  (session session)
  (counter-path path)
  (counter (:pointer (:struct performance-metrics-counter-meta))))

;; commands for XR_HTC_foveation
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-limit-reached)
(defextfun ("xrApplyFoveationHTC" apply-foveation-htc 158) result
  (session session)
  (apply-info (:pointer (:struct foveation-apply-info-htc))))

;; commands for XR_ML_compat
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached :error-pose-invalid)
(defextfun ("xrCreateSpaceFromCoordinateFrameUIDML" create-space-from-coordinate-frame-uid-ml 159) result
  (session session)
  (create-info (:pointer (:struct coordinate-space-create-info-ml)))
  (space (:pointer space)))

;; XR_FB_haptic_pcm
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-handle-invalid :error-instance-lost :error-session-lost
;;                 :error-path-unsupported :error-path-invalid
;;                 :error-action-type-mismatch :error-actionset-not-attached)
(defextfun ("xrGetDeviceSampleRateFB" get-device-sample-rate-fb 160) result
  (session session)
  (haptic-action-info (:pointer (:struct haptic-action-info)))
  (device-sample-rate (:pointer (:struct device-pcm-sample-rate-get-info-fb))))

;; XR_QCOM_tracking_optimization_settings
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost
;;                 :error-hint-already-set-qcom)
(defextfun ("xrSetTrackingOptimizationSettingsHintQCOM" set-tracking-optimization-settings-hint-qcom 161) result
  (session session)
  (domain tracking-optimization-settings-domain-qcom)
  (hint tracking-optimization-settings-hint-qcom))

;; commands for XR_FB_spatial_entity_user
;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost :error-out-of-memory
;;                 :error-limit-reached)
(defextfun ("xrCreateSpaceUserFB" create-space-user-fb 162) result
  (session session)
  (info (:pointer (:struct space-user-create-info-fb)))
  (user (:pointer space-user-fb)))

;; success codes: (:success :session-loss-pending)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrGetSpaceUserIdFB" get-space-user-id-fb 163) result
  (user space-user-fb)
  (user-id (:pointer space-user-id-fb)))

;; success codes: (:success)
;;   error codes: (:error-function-unsupported :error-runtime-failure
;;                 :error-handle-invalid)
(defextfun ("xrDestroySpaceUserFB" destroy-space-user-fb 164) result
  ;; externsync = "true_with_children"
  (user space-user-fb))

;; commands for XR_MNDX_force_feedback_curl
;; success codes: (:success :session-loss-pending :session-not-focused)
;;   error codes: (:error-function-unsupported :error-validation-failure
;;                 :error-runtime-failure :error-handle-invalid
;;                 :error-instance-lost :error-session-lost)
(defextfun ("xrApplyForceFeedbackCurlMNDX" apply-force-feedback-curl-mndx 165) result
  (hand-tracker hand-tracker-ext)
  (locations (:pointer (:struct force-feedback-curl-apply-locations-mndx))))

