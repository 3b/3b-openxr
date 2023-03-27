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
;; types :
(defctype bool-32 :uint32)

(defctype flags-64 :uint64)

(defctype time :int64)

(defctype duration :int64)

(defctype version :uint64)

(defctype space-user-id-fb :uint64)

(defctype path atom)

(defctype system-id atom)

(defctype controller-model-key-msft atom)

(defctype async-request-id-fb atom)

(defctype render-model-key-fb atom)

(defbitfield instance-create-flags)

(defbitfield session-create-flags)

(defbitfield swapchain-create-flags
  ;; content will be protected from cpu access
  (:protected-content #x00000001)
  ;; only one image will be acquired from this swapchain over its lifetime
  (:static-image #x00000002))

(defbitfield swapchain-usage-flags
  ;; specifies that the image may: be a color rendering target.
  (:color-attachment #x00000001)
  ;; specifies that the image may: be a depth/stencil rendering target.
  (:depth-stencil-attachment #x00000002)
  ;; specifies that the image may: be accessed out of order and that access may: be via atomic operations.
  (:unordered-access #x00000004)
  ;; specifies that the image may: be used as the source of a transfer operation.
  (:transfer-src #x00000008)
  ;; specifies that the image may: be used as the destination of a transfer operation.
  (:transfer-dst #x00000010)
  ;; specifies that the image may: be sampled by a shader.
  (:sampled #x00000020)
  ;; specifies that the image may: be reinterpreted as another image format.
  (:mutable-format #x00000040)
  ;; specifies that the image may: be used as a input attachment.
  (:input-attachment-mnd #x00000080)
  ;; alias xr_swapchain_usage_input_attachment_bit_khr
  ;;    -> xr_swapchain_usage_input_attachment_bit_mnd
  ;; specifies that the image may: be used as a input attachment.
  (:input-attachment-khr #x00000080))

(defbitfield view-state-flags
  ;; indicates validity of all slink:xrview orientations
  (:orientation-valid #x00000001)
  ;; indicates validity of all slink:xrview positions
  (:position-valid #x00000002)
  ;; indicates whether all slink:xrview orientations are actively tracked
  (:orientation-tracked #x00000004)
  ;; indicates whether all slink:xrview positions are actively tracked
  (:position-tracked #x00000008))

(defbitfield composition-layer-flags
  ;; enables chromatic aberration correction when not done by default. this flag has no effect on any known conformant runtime, and is planned for deprecation for openxr 1.1
  (:correct-chromatic-aberration #x00000001)
  ;; enables the layer texture alpha channel.
  (:blend-texture-source-alpha #x00000002)
  ;; indicates the texture color channels have not been premultiplied by the texture alpha channel.
  (:unpremultiplied-alpha #x00000004))

(defbitfield space-location-flags
  ;; indicates that the pname:orientation member contains valid data
  (:orientation-valid #x00000001)
  ;; indicates that the pname:position member contains valid data
  (:position-valid #x00000002)
  ;; indicates whether pname:pose member contains an actively tracked pname:orientation
  (:orientation-tracked #x00000004)
  ;; indicates whether pname:pose member contains an actively tracked pname:position
  (:position-tracked #x00000008))

(defbitfield space-velocity-flags
  ;; indicates that the pname:linearvelocity member contains valid data. applications must: not read the pname:linearvelocity field if this flag is unset.
  (:linear-valid #x00000001)
  ;; indicates that the pname:angularvelocity member contains valid data. applications must: not read the pname:angularvelocity field if this flag is unset.
  (:angular-valid #x00000002))

(defbitfield input-source-localized-name-flags
  ;; asks for the part of the string which indicates the top level user path the source represents
  (:user-path #x00000001)
  ;; asks for the part of the string which represents the interaction profile of the source
  (:interaction-profile #x00000002)
  ;; asks for the part of the string which represents the component on the device which needs to be interacted with
  (:component #x00000004))

(defbitfield vulkan-instance-create-flags-khr)

(defbitfield vulkan-device-create-flags-khr)

(defbitfield debug-utils-message-severity-flags-ext
  ;; most verbose output severity, typically used for debugging.
  (:verbose-ext #x00000001)
  ;; general info message
  (:info-ext #x00000010)
  ;; indicates the item may be the cause of issues.
  (:warning-ext #x00000100)
  ;; indicates that the item is definitely related to erroneous behavior.
  (:error-ext #x00001000))

(defbitfield debug-utils-message-type-flags-ext
  ;; indicates this is a general message
  (:general-ext #x00000001)
  ;; indicates the message is related to a validation message
  (:validation-ext #x00000002)
  ;; indicates the message is related to a potential performance situation
  (:performance-ext #x00000004)
  ;; indicates the message is related to a non-conformant runtime result
  (:conformance-ext #x00000008))

(defbitfield overlay-main-session-flags-extx
  ;; indicates the main session enabled `xr_khr_composition_layer_depth`
  (:enabled-composition-layer-info-depth-extx #x00000001))

(defbitfield overlay-session-create-flags-extx)

(defbitfield android-surface-swapchain-flags-fb
  ;; create the underlying bufferqueue in synchronous mode
  (:synchronous-fb #x00000001)
  ;; acquire most recent buffer whose presentation timestamp is not greater than display time of final composited frame
  (:use-timestamps-fb #x00000002))

(defbitfield composition-layer-image-layout-flags-fb
  ;; the coordinate origin of the swapchain image must be considered to be flipped vertically.
  (:vertical-flip-fb #x00000001))

(defbitfield composition-layer-secure-content-flags-fb
  ;; indicates the layer will only be visible inside the hmd, and not visible to external sources
  (:exclude-layer-fb #x00000001)
  ;; indicates the layer will be displayed inside the hmd, but replaced by proxy content when written to external sources
  (:replace-layer-fb #x00000002))

(defbitfield swapchain-create-foveation-flags-fb
  ;; explicitly create the swapchain with scaled bin foveation support. the application must ensure that the swapchain is using the opengl graphics api and that the qcom_texture_foveated extension is supported and enabled.
  (:scaled-bin-fb #x00000001)
  ;; explicitly create the swapchain with fragment density map foveation support. the application must ensure that the swapchain is using the vulkan graphics api and that the vk_ext_fragment_density_map extension is supported and enabled.
  (:ragment-density-map-fb #x00000002))

(defbitfield swapchain-state-foveation-flags-fb)

(defbitfield foveation-eye-tracked-profile-create-flags-meta)

(defbitfield foveation-eye-tracked-state-flags-meta
  ;; indicates whether or not foveation data is valid. this can happen if the eye tracker is obscured, the camera has dirt, or eye lid is closed, etc.
  (:valid-meta #x00000001))

(defbitfield triangle-mesh-flags-fb
  ;; the triangle mesh is mutable (can be modified after it is created).
  (:mutable-fb #x00000001))

(defbitfield passthrough-flags-fb
  ;; the object (passthrough, layer) is running at creation.
  (:is-running-at-creation-fb #x00000001)
  ;; the passthrough system sends depth information to the compositor. only applicable to layer objects.
  (:layer-depth-fb #x00000002))

(defbitfield passthrough-state-changed-flags-fb
  ;; passthrough system requires reinitialization.
  (:reinit-required-fb #x00000001)
  ;; non-recoverable error has occurred. a device reboot or a firmware update may be required.
  (:non-recoverable-error-fb #x00000002)
  ;; a recoverable error has occurred. the runtime will attempt to recover, but some functionality may be temporarily unavailable.
  (:recoverable-error-fb #x00000004)
  ;; the runtime has recovered from a previous error and is functioning normally.
  (:restored-error-fb #x00000008))

(defbitfield passthrough-capability-flags-fb
  ;; the system supports passthrough.
  (:bit-fb #x00000001)
  ;; the system can show passthrough with realistic colors. ename:xr_passthrough_capability_bit_fb must: be set if ename:xr_passthrough_capability_color_bit_fb is set.
  (:color-fb #x00000002)
  ;; the system supports passthrough layers composited using depth testing. ename:xr_passthrough_capability_bit_fb must: be set if ename:xr_passthrough_capability_layer_depth_bit_fb is set.
  (:layer-depth-fb #x00000004))

(defbitfield hand-tracking-aim-flags-fb
  ;; aiming data is computed from additional sources beyond the hand data in the base structure
  (:computed-fb #x00000001)
  ;; aiming data is valid
  (:valid-fb #x00000002)
  ;; index finger pinch discrete signal
  (:index-pinching-fb #x00000004)
  ;; middle finger pinch discrete signal
  (:middle-pinching-fb #x00000008)
  ;; ring finger pinch discrete signal
  (:ring-pinching-fb #x00000010)
  ;; little finger pinch discrete signal
  (:little-pinching-fb #x00000020)
  ;; system gesture is active
  (:system-gesture-fb #x00000040)
  ;; hand is currently marked as dominant for the system
  (:dominant-hand-fb #x00000080)
  ;; system menu gesture is active
  (:menu-pressed-fb #x00000100))

(defbitfield keyboard-tracking-flags-fb
  ;; indicates that the system has a physically tracked keyboard to report.  if not set then no other bits should be considered to be valid or meaningful.  if set either xr_keyboard_tracking_local_bit_fb or xr_keyboard_tracking_remote_bit_fb must also be set.
  (:exists-fb #x00000001)
  ;; indicates that the physically tracked keyboard is intended to be used in a local pairing with the system.  mutally exclusive with xr_keyboard_tracking_remote_bit_fb.
  (:local-fb #x00000002)
  ;; indicates that the physically tracked keyboard is intended to be used while paired to a separate remote computing device. mutally exclusive with xr_keyboard_tracking_local_bit_fb.
  (:remote-fb #x00000004)
  ;; indicates that the physically tracked keyboard is actively connected to the headset and capable of sending key data
  (:connected-fb #x00000008))

(defbitfield keyboard-tracking-query-flags-fb
  ;; indicates the query is for the physically tracked keyboard that is intended to be used in a local pairing with the system. mutally exclusive with xr_keyboard_tracking_query_remote_bit_fb.
  (:local-fb #x00000002)
  ;; indicates the query is for the physically tracked keyboard that may be connected to a separate remote computing device. mutally exclusive with xr_keyboard_tracking_query_local_bit_fb.
  (:remote-fb #x00000004))

(defbitfield composition-layer-space-warp-info-flags-fb
  ;; skip current frame's space warp extrapolation
  (:rame-skip-fb #x00000001))

(defbitfield render-model-flags-fb
  ;; minimal level of support.  can only contain a single mesh.  can only contain a single texture.  can not contain transparency.  assumes unlit rendering.  requires extension khr_texturebasisu.
  (:supports-gltf-2-0-subset-1-fb #x00000001)
  ;; all of xr_render_model_supports_gltf_2_0_subset_1_bit_fb support plus: multiple meshes. multiple textures. texture transparency.
  (:supports-gltf-2-0-subset-2-fb #x00000002))

(defbitfield digital-lens-control-flags-almalence
  ;; disables digital lens processing of render textures
  (:processing-disable-almalence #x00000001))

(defbitfield composition-layer-settings-flags-fb
  ;; indicates compositor may: use layer texture supersampling.
  (:normal-super-sampling-fb #x00000001)
  ;; indicates compositor may: use high quality layer texture supersampling.
  (:quality-super-sampling-fb #x00000002)
  ;; indicates compositor may: use layer texture sharpening.
  (:normal-sharpening-fb #x00000004)
  ;; indicates compositor may: use high quality layer texture sharpening.
  (:quality-sharpening-fb #x00000008))

(defbitfield external-camera-status-flags-oculus
  ;; external camera is connected
  (:connected-oculus #x00000001)
  ;; external camera is undergoing calibration
  (:calibrating-oculus #x00000002)
  ;; external camera has tried and failed calibration
  (:calibration-failed-oculus #x00000004)
  ;; external camera has tried and passed calibration
  (:calibrated-oculus #x00000008)
  ;; external camera is capturing
  (:capturing-oculus #x00000010))

(defbitfield performance-metrics-counter-flags-meta
  ;; indicates any of the values in xrperformancemetricscountermeta is valid.
  (:any-value-valid-meta #x00000001)
  ;; indicates the uintvalue in xrperformancemetricscountermeta is valid.
  (:uint-value-valid-meta #x00000002)
  ;; indicates the floatvalue in xrperformancemetricscountermeta is valid.
  (:oat-value-valid-meta #x00000004))

(defbitfield foveation-dynamic-flags-htc
  ;; allow system to set periphery pixel density dynamically.
  (:level-enabled-htc #x00000001)
  ;; allow system to set clear fov degree dynamically.
  (:clear-fov-enabled-htc #x00000002)
  ;; allow system to set focal center offset dynamically.
  (:ocal-center-offset-enabled-htc #x00000004))

(defbitfield frame-end-info-flags-ml
  ;; indicates that the content for this frame is protected and should not be recorded or captured outside the graphics system.
  (:protected-ml #x00000001)
  ;; indicates that a soft fade to transparent should be added to the frame in the compositor to blend any hard edges at the fov limits.
  (:vignette-ml #x00000002))

(defbitfield global-dimmer-frame-end-info-flags-ml
  ;; indicates that the global dimmer should: be enabled and controlled by slink:xrglobaldimmerframeendinfoml::pname:dimmervalue.
  (:enabled-ml #x00000001))

(defctype instance xr-handle)
(defctype session xr-handle)
(defctype action-set xr-handle)
(defctype action xr-handle)
(defctype swapchain xr-handle)
(defctype space xr-handle)
(defctype debug-utils-messenger-ext xr-handle)
(defctype spatial-anchor-msft xr-handle)
(defctype hand-tracker-ext xr-handle)
(defctype foveation-profile-fb xr-handle)
(defctype triangle-mesh-fb xr-handle)
(defctype passthrough-fb xr-handle)
(defctype passthrough-layer-fb xr-handle)
(defctype geometry-instance-fb xr-handle)
(defctype facial-tracker-htc xr-handle)
(defctype passthrough-htc xr-handle)
(defctype face-tracker-fb xr-handle)
(defctype body-tracker-fb xr-handle)
(defctype eye-tracker-fb xr-handle)
(defctype space-user-fb xr-handle)
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
  (:type-composition-layer-cube-khr 10000006000)
  (:type-instance-create-info-android-khr 10000008000)
  (:type-composition-layer-depth-info-khr 10000010000)
  (:type-vulkan-swapchain-format-list-create-info-khr 10000014000)
  (:type-event-data-perf-settings-ext 10000015000)
  (:type-composition-layer-cylinder-khr 10000017000)
  (:type-composition-layer-equirect-khr 10000018000)
  (:type-debug-utils-object-name-info-ext 10000019000)
  (:type-debug-utils-messenger-callback-data-ext 10000019001)
  (:type-debug-utils-messenger-create-info-ext 10000019002)
  (:type-debug-utils-label-ext 10000019003)
  (:type-graphics-binding-opengl-win32-khr 10000023000)
  (:type-graphics-binding-opengl-xlib-khr 10000023001)
  (:type-graphics-binding-opengl-xcb-khr 10000023002)
  (:type-graphics-binding-opengl-wayland-khr 10000023003)
  (:type-swapchain-image-opengl-khr 10000023004)
  (:type-graphics-requirements-opengl-khr 10000023005)
  (:type-graphics-binding-opengl-es-android-khr 10000024001)
  (:type-swapchain-image-opengl-es-khr 10000024002)
  (:type-graphics-requirements-opengl-es-khr 10000024003)
  (:type-graphics-binding-vulkan-khr 10000025000)
  (:type-swapchain-image-vulkan-khr 10000025001)
  (:type-graphics-requirements-vulkan-khr 10000025002)
  (:type-graphics-binding-d3d11-khr 10000027000)
  (:type-swapchain-image-d3d11-khr 10000027001)
  (:type-graphics-requirements-d3d11-khr 10000027002)
  (:type-graphics-binding-d3d12-khr 10000028000)
  (:type-swapchain-image-d3d12-khr 10000028001)
  (:type-graphics-requirements-d3d12-khr 10000028002)
  (:type-system-eye-gaze-interaction-properties-ext 10000030000)
  (:type-eye-gaze-sample-time-ext 10000030001)
  (:type-visibility-mask-khr 10000031000)
  (:type-event-data-visibility-mask-changed-khr 10000031001)
  (:type-session-create-info-overlay-extx 10000033000)
  (:type-event-data-main-session-visibility-changed-extx 10000033003)
  (:type-composition-layer-color-scale-bias-khr 10000034000)
  (:type-spatial-anchor-create-info-msft 10000039000)
  (:type-spatial-anchor-space-create-info-msft 10000039001)
  (:type-composition-layer-image-layout-fb 10000040000)
  (:type-composition-layer-alpha-blend-fb 10000041001)
  (:type-view-configuration-depth-range-ext 10000046000)
  (:type-graphics-binding-egl-mndx 10000048004)
  (:type-spatial-graph-node-space-create-info-msft 10000049000)
  (:type-spatial-graph-static-node-binding-create-info-msft 10000049001)
  (:type-spatial-graph-node-binding-properties-get-info-msft 10000049002)
  (:type-spatial-graph-node-binding-properties-msft 10000049003)
  (:type-system-hand-tracking-properties-ext 10000051000)
  (:type-hand-tracker-create-info-ext 10000051001)
  (:type-hand-joints-locate-info-ext 10000051002)
  (:type-hand-joint-locations-ext 10000051003)
  (:type-hand-joint-velocities-ext 10000051004)
  (:type-system-hand-tracking-mesh-properties-msft 10000052000)
  (:type-hand-mesh-space-create-info-msft 10000052001)
  (:type-hand-mesh-update-info-msft 10000052002)
  (:type-hand-mesh-msft 10000052003)
  (:type-hand-pose-type-info-msft 10000052004)
  (:type-secondary-view-configuration-session-begin-info-msft 10000053000)
  (:type-secondary-view-configuration-state-msft 10000053001)
  (:type-secondary-view-configuration-frame-state-msft 10000053002)
  (:type-secondary-view-configuration-frame-end-info-msft 10000053003)
  (:type-secondary-view-configuration-layer-info-msft 10000053004)
  (:type-secondary-view-configuration-swapchain-create-info-msft 10000053005)
  (:type-controller-model-key-state-msft 10000055000)
  (:type-controller-model-node-properties-msft 10000055001)
  (:type-controller-model-properties-msft 10000055002)
  (:type-controller-model-node-state-msft 10000055003)
  (:type-controller-model-state-msft 10000055004)
  (:type-view-configuration-view-fov-epic 10000059000)
  (:type-holographic-window-attachment-msft 10000063000)
  (:type-composition-layer-reprojection-info-msft 10000066000)
  (:type-composition-layer-reprojection-plane-override-msft 10000066001)
  (:type-android-surface-swapchain-create-info-fb 10000070000)
  (:type-composition-layer-secure-content-fb 10000072000)
  (:type-body-tracker-create-info-fb 10000076001)
  (:type-body-joints-locate-info-fb 10000076002)
  (:type-system-body-tracking-properties-fb 10000076004)
  (:type-body-joint-locations-fb 10000076005)
  (:type-body-skeleton-fb 10000076006)
  (:type-interaction-profile-dpad-binding-ext 10000078000)
  (:type-interaction-profile-analog-threshold-valve 10000079000)
  (:type-hand-joints-motion-range-info-ext 10000080000)
  (:type-loader-init-info-android-khr 10000089000)
  (:type-vulkan-instance-create-info-khr 10000090000)
  (:type-vulkan-device-create-info-khr 10000090001)
  (:type-vulkan-graphics-device-get-info-khr 10000090003)
  ;; alias xr_type_graphics_binding_vulkan2_khr
  ;;    -> xr_type_graphics_binding_vulkan_khr
  (:type-graphics-binding-vulkan2-khr 10000025000)
  ;; alias xr_type_swapchain_image_vulkan2_khr
  ;;    -> xr_type_swapchain_image_vulkan_khr
  (:type-swapchain-image-vulkan2-khr 10000025001)
  ;; alias xr_type_graphics_requirements_vulkan2_khr
  ;;    -> xr_type_graphics_requirements_vulkan_khr
  (:type-graphics-requirements-vulkan2-khr 10000025002)
  (:type-composition-layer-equirect2-khr 10000091000)
  (:type-scene-observer-create-info-msft 10000097000)
  (:type-scene-create-info-msft 10000097001)
  (:type-new-scene-compute-info-msft 10000097002)
  (:type-visual-mesh-compute-lod-info-msft 10000097003)
  (:type-scene-components-msft 10000097004)
  (:type-scene-components-get-info-msft 10000097005)
  (:type-scene-component-locations-msft 10000097006)
  (:type-scene-components-locate-info-msft 10000097007)
  (:type-scene-objects-msft 10000097008)
  (:type-scene-component-parent-filter-info-msft 10000097009)
  (:type-scene-object-types-filter-info-msft 10000097010)
  (:type-scene-planes-msft 10000097011)
  (:type-scene-plane-alignment-filter-info-msft 10000097012)
  (:type-scene-meshes-msft 10000097013)
  (:type-scene-mesh-buffers-get-info-msft 10000097014)
  (:type-scene-mesh-buffers-msft 10000097015)
  (:type-scene-mesh-vertex-buffer-msft 10000097016)
  (:type-scene-mesh-indices-uint32-msft 10000097017)
  (:type-scene-mesh-indices-uint16-msft 10000097018)
  (:type-serialized-scene-fragment-data-get-info-msft 10000098000)
  (:type-scene-deserialize-info-msft 10000098001)
  (:type-event-data-display-refresh-rate-changed-fb 10000101000)
  (:type-vive-tracker-paths-htcx 10000103000)
  (:type-event-data-vive-tracker-connected-htcx 10000103001)
  (:type-system-facial-tracking-properties-htc 10000104000)
  (:type-facial-tracker-create-info-htc 10000104001)
  (:type-facial-expressions-htc 10000104002)
  (:type-system-color-space-properties-fb 10000108000)
  (:type-hand-tracking-mesh-fb 10000110001)
  (:type-hand-tracking-scale-fb 10000110003)
  (:type-hand-tracking-aim-state-fb 10000111001)
  (:type-hand-tracking-capsules-state-fb 10000112000)
  (:type-system-spatial-entity-properties-fb 10000113004)
  (:type-spatial-anchor-create-info-fb 10000113003)
  (:type-space-component-status-set-info-fb 10000113007)
  (:type-space-component-status-fb 10000113001)
  (:type-event-data-spatial-anchor-create-complete-fb 10000113005)
  (:type-event-data-space-set-status-complete-fb 10000113006)
  (:type-foveation-profile-create-info-fb 10000114000)
  (:type-swapchain-create-info-foveation-fb 10000114001)
  (:type-swapchain-state-foveation-fb 10000114002)
  (:type-foveation-level-profile-create-info-fb 10000115000)
  (:type-keyboard-space-create-info-fb 10000116009)
  (:type-keyboard-tracking-query-fb 10000116004)
  (:type-system-keyboard-tracking-properties-fb 10000116002)
  (:type-triangle-mesh-create-info-fb 10000117001)
  (:type-system-passthrough-properties-fb 10000118000)
  (:type-passthrough-create-info-fb 10000118001)
  (:type-passthrough-layer-create-info-fb 10000118002)
  (:type-composition-layer-passthrough-fb 10000118003)
  (:type-geometry-instance-create-info-fb 10000118004)
  (:type-geometry-instance-transform-fb 10000118005)
  (:type-system-passthrough-properties2-fb 10000118006)
  (:type-passthrough-style-fb 10000118020)
  (:type-passthrough-color-map-mono-to-rgba-fb 10000118021)
  (:type-passthrough-color-map-mono-to-mono-fb 10000118022)
  (:type-passthrough-brightness-contrast-saturation-fb 10000118023)
  (:type-event-data-passthrough-state-changed-fb 10000118030)
  (:type-render-model-path-info-fb 10000119000)
  (:type-render-model-properties-fb 10000119001)
  (:type-render-model-buffer-fb 10000119002)
  (:type-render-model-load-info-fb 10000119003)
  (:type-system-render-model-properties-fb 10000119004)
  (:type-render-model-capabilities-request-fb 10000119005)
  (:type-binding-modifications-khr 10000120000)
  (:type-view-locate-foveated-rendering-varjo 10000121000)
  (:type-foveated-view-configuration-view-varjo 10000121001)
  (:type-system-foveated-rendering-properties-varjo 10000121002)
  (:type-composition-layer-depth-test-varjo 10000122000)
  (:type-system-marker-tracking-properties-varjo 10000124000)
  (:type-event-data-marker-tracking-update-varjo 10000124001)
  (:type-marker-space-create-info-varjo 10000124002)
  (:type-frame-end-info-ml 10000135000)
  (:type-global-dimmer-frame-end-info-ml 10000136000)
  (:type-coordinate-space-create-info-ml 10000137000)
  (:type-spatial-anchor-persistence-info-msft 10000142000)
  (:type-spatial-anchor-from-persisted-anchor-create-info-msft 10000142001)
  (:type-space-query-info-fb 10000156001)
  (:type-space-query-results-fb 10000156002)
  (:type-space-storage-location-filter-info-fb 10000156003)
  (:type-space-uuid-filter-info-fb 10000156054)
  (:type-space-component-filter-info-fb 10000156052)
  (:type-event-data-space-query-results-available-fb 10000156103)
  (:type-event-data-space-query-complete-fb 10000156104)
  (:type-space-save-info-fb 10000158000)
  (:type-space-erase-info-fb 10000158001)
  (:type-event-data-space-save-complete-fb 10000158106)
  (:type-event-data-space-erase-complete-fb 10000158107)
  (:type-swapchain-image-foveation-vulkan-fb 10000160000)
  (:type-swapchain-state-android-surface-dimensions-fb 10000161000)
  (:type-swapchain-state-sampler-opengl-es-fb 10000162000)
  (:type-swapchain-state-sampler-vulkan-fb 10000163000)
  (:type-space-share-info-fb 10000169001)
  (:type-event-data-space-share-complete-fb 10000169002)
  (:type-composition-layer-space-warp-info-fb 10000171000)
  (:type-system-space-warp-properties-fb 10000171001)
  (:type-haptic-amplitude-envelope-vibration-fb 10000173001)
  (:type-semantic-labels-fb 10000175000)
  (:type-room-layout-fb 10000175001)
  (:type-boundary-2d-fb 10000175002)
  (:type-digital-lens-control-almalence 10000196000)
  (:type-event-data-scene-capture-complete-fb 10000198001)
  (:type-scene-capture-request-info-fb 10000198050)
  (:type-space-container-fb 10000199000)
  (:type-foveation-eye-tracked-profile-create-info-meta 10000200000)
  (:type-foveation-eye-tracked-state-meta 10000200001)
  (:type-system-foveation-eye-tracked-properties-meta 10000200002)
  (:type-system-face-tracking-properties-fb 10000201004)
  (:type-face-tracker-create-info-fb 10000201005)
  (:type-face-expression-info-fb 10000201002)
  (:type-face-expression-weights-fb 10000201006)
  (:type-eye-tracker-create-info-fb 10000202001)
  (:type-eye-gazes-info-fb 10000202002)
  (:type-eye-gazes-fb 10000202003)
  (:type-system-eye-tracking-properties-fb 10000202004)
  (:type-passthrough-keyboard-hands-intensity-fb 10000203002)
  (:type-composition-layer-settings-fb 10000204000)
  (:type-haptic-pcm-vibration-fb 10000209001)
  (:type-device-pcm-sample-rate-state-fb 10000209002)
  ;; alias xr_type_device_pcm_sample_rate_get_info_fb
  ;;    -> xr_type_device_pcm_sample_rate_state_fb
  (:type-device-pcm-sample-rate-get-info-fb 10000209002)
  (:type-composition-layer-depth-test-fb 10000212000)
  (:type-local-dimming-frame-end-info-meta 10000216000)
  (:type-external-camera-oculus 10000226000)
  (:type-vulkan-swapchain-create-info-meta 10000227000)
  (:type-performance-metrics-state-meta 10000232001)
  (:type-performance-metrics-counter-meta 10000232002)
  (:type-space-list-save-info-fb 10000238000)
  (:type-event-data-space-list-save-complete-fb 10000238001)
  (:type-space-user-create-info-fb 10000241001)
  (:type-system-headset-id-properties-meta 10000245000)
  (:type-passthrough-create-info-htc 10000317001)
  (:type-passthrough-color-htc 10000317002)
  (:type-passthrough-mesh-transform-info-htc 10000317003)
  (:type-composition-layer-passthrough-htc 10000317004)
  (:type-foveation-apply-info-htc 10000318000)
  (:type-foveation-dynamic-mode-info-htc 10000318001)
  (:type-foveation-custom-mode-info-htc 10000318002)
  (:type-active-action-set-priorities-ext 10000373000)
  (:type-system-force-feedback-curl-properties-mndx 10000375000)
  (:type-force-feedback-curl-apply-locations-mndx 10000375001))

;; Error and return codes
(defcenum result
  ;; function successfully completed.
  (:success 0)
  ;; the specified timeout time occurred before the operation could complete.
  (:timeout-expired 1)
  ;; the session will be lost soon.
  (:session-loss-pending 3)
  ;; no event was available.
  (:event-unavailable 4)
  ;; the space's bounds are not known at the moment.
  (:space-bounds-unavailable 7)
  ;; the session is not in the focused state.
  (:session-not-focused 8)
  ;; a frame has been discarded from composition.
  (:frame-discarded 9)
  ;; the function usage was invalid in some way.
  (:error-validation-failure -1)
  ;; the runtime failed to handle the function in an unexpected way that is not covered by another error result. 
  (:error-runtime-failure -2)
  ;; a memory allocation has failed.
  (:error-out-of-memory -3)
  ;; the runtime does not support the requested api version.
  (:error-api-version-unsupported -4)
  ;; initialization of object could not be completed.
  (:error-initialization-failed -6)
  ;; the requested function was not found or is otherwise unsupported.
  (:error-function-unsupported -7)
  ;; the requested feature is not supported.
  (:error-feature-unsupported -8)
  ;; a requested extension is not supported.
  (:error-extension-not-present -9)
  ;; the runtime supports no more of the requested resource.
  (:error-limit-reached -10)
  ;; the supplied size was smaller than required.
  (:error-size-insufficient -11)
  ;; a supplied object handle was invalid.
  (:error-handle-invalid -12)
  ;; the slink:xrinstance was lost or could not be found. it will need to be destroyed and optionally recreated.
  (:error-instance-lost -13)
  ;; the session <<session_running, is already running>>.
  (:error-session-running -14)
  ;; the session <<session_not_running, is not yet running>>.
  (:error-session-not-running -16)
  ;; the slink:xrsession was lost. it will need to be destroyed and optionally recreated.
  (:error-session-lost -17)
  ;; the provided basetype:xrsystemid was invalid.
  (:error-system-invalid -18)
  ;; the provided basetype:xrpath was not valid.
  (:error-path-invalid -19)
  ;; the maximum number of supported semantic paths has been reached.
  (:error-path-count-exceeded -20)
  ;; the semantic path character format is invalid.
  (:error-path-format-invalid -21)
  ;; the semantic path is unsupported.
  (:error-path-unsupported -22)
  ;; the layer was null or otherwise invalid.
  (:error-layer-invalid -23)
  ;; the number of specified layers is greater than the supported number.
  (:error-layer-limit-exceeded -24)
  ;; the image rect was negatively sized or otherwise invalid.
  (:error-swapchain-rect-invalid -25)
  ;; the image format is not supported by the runtime or platform.
  (:error-swapchain-format-unsupported -26)
  ;; the api used to retrieve an action's state does not match the action's type.
  (:error-action-type-mismatch -27)
  ;; the session is not in the ready state.
  (:error-session-not-ready -28)
  ;; the session is not in the stopping state.
  (:error-session-not-stopping -29)
  ;; the provided basetype:xrtime was zero, negative, or out of range.
  (:error-time-invalid -30)
  ;; the specified reference space is not supported by the runtime or system.
  (:error-reference-space-unsupported -31)
  ;; the file could not be accessed.
  (:error-file-access-error -32)
  ;; the file's contents were invalid.
  (:error-file-contents-invalid -33)
  ;; the specified form factor is not supported by the current runtime or platform.
  (:error-form-factor-unsupported -34)
  ;; the specified form factor is supported, but the device is currently not available, e.g. not plugged in or powered off.
  (:error-form-factor-unavailable -35)
  ;; a requested api layer is not present or could not be loaded.
  (:error-api-layer-not-present -36)
  ;; the call was made without having made a previously required call.
  (:error-call-order-invalid -37)
  ;; the given graphics device is not in a valid state. the graphics device could be lost or initialized without meeting graphics requirements.
  (:error-graphics-device-invalid -38)
  ;; the supplied pose was invalid with respect to the requirements.
  (:error-pose-invalid -39)
  ;; the supplied index was outside the range of valid indices.
  (:error-index-out-of-range -40)
  ;; the specified view configuration type is not supported by the runtime or platform.
  (:error-view-configuration-type-unsupported -41)
  ;; the specified environment blend mode is not supported by the runtime or platform.
  (:error-environment-blend-mode-unsupported -42)
  ;; the name provided was a duplicate of an already-existing resource.
  (:error-name-duplicated -44)
  ;; the name provided was invalid.
  (:error-name-invalid -45)
  ;; a referenced action set is not attached to the session.
  (:error-actionset-not-attached -46)
  ;; the session already has attached action sets.
  (:error-actionsets-already-attached -47)
  ;; the localized name provided was a duplicate of an already-existing resource.
  (:error-localized-name-duplicated -48)
  ;; the localized name provided was invalid.
  (:error-localized-name-invalid -49)
  ;; the fname:xrgetgraphicsrequirements* call was not made before calling fname:xrcreatesession.
  (:error-graphics-requirements-call-missing -50)
  ;; the loader was unable to find or load a runtime.
  (:error-runtime-unavailable -51)
  ;; xrsetandroidapplicationthreadkhr failed as thread id is invalid.
  (:error-android-thread-settings-id-invalid-khr -10000003000)
  ;; xrsetandroidapplicationthreadkhr failed setting the thread attributes/priority.
  (:error-android-thread-settings-failure-khr -10000003001)
  ;; spatial anchor could not be created at that location.
  (:error-create-spatial-anchor-failed-msft -10000039001)
  ;; the secondary view configuration was not enabled when creating the session.
  (:error-secondary-view-configuration-type-not-enabled-msft -10000053000)
  ;; the controller model key is invalid.
  (:error-controller-model-key-invalid-msft -10000055000)
  ;; the reprojection mode is not supported.
  (:error-reprojection-mode-unsupported-msft -10000066000)
  ;; compute new scene not completed.
  (:error-compute-new-scene-not-completed-msft -10000097000)
  ;; scene component id invalid.
  (:error-scene-component-id-invalid-msft -10000097001)
  ;; scene component type mismatch.
  (:error-scene-component-type-mismatch-msft -10000097002)
  ;; scene mesh buffer id invalid.
  (:error-scene-mesh-buffer-id-invalid-msft -10000097003)
  ;; scene compute feature incompatible.
  (:error-scene-compute-feature-incompatible-msft -10000097004)
  ;; scene compute consistency mismatch.
  (:error-scene-compute-consistency-mismatch-msft -10000097005)
  ;; the display refresh rate is not supported by the platform.
  (:error-display-refresh-rate-unsupported-fb -10000101000)
  ;; the color space is not supported by the runtime.
  (:error-color-space-unsupported-fb -10000108000)
  ;; the component type is not supported for this space.
  (:error-space-component-not-supported-fb -10000113000)
  ;; the required component is not enabled for this space.
  (:error-space-component-not-enabled-fb -10000113001)
  ;; a request to set the component's status is currently pending.
  (:error-space-component-status-pending-fb -10000113002)
  ;; the component is already set to the requested value.
  (:error-space-component-status-already-set-fb -10000113003)
  ;; the object state is unexpected for the issued command.
  (:error-unexpected-state-passthrough-fb -10000118000)
  ;; trying to create an mr feature when one was already created and only one instance is allowed.
  (:error-feature-already-created-passthrough-fb -10000118001)
  ;; requested functionality requires a feature to be created first.
  (:error-feature-required-passthrough-fb -10000118002)
  ;; requested functionality is not permitted - application is not allowed to perform the requested operation.
  (:error-not-permitted-passthrough-fb -10000118003)
  ;; there weren't sufficient resources available to perform an operation.
  (:error-insufficient-resources-passthrough-fb -10000118004)
  ;; unknown passthrough error (no further details provided).
  (:error-unknown-passthrough-fb -10000118050)
  ;; the model key is invalid.
  (:error-render-model-key-invalid-fb -10000119000)
  ;; the model is unavailable.
  (:nder-model-unavailable-fb 10000119020)
  ;; marker tracking is disabled or the specified marker is not currently tracked.
  (:error-marker-not-tracked-varjo -10000124000)
  ;; the specified marker id is not valid.
  (:error-marker-id-invalid-varjo -10000124001)
  ;; a spatial anchor was not found associated with the spatial anchor name provided
  (:error-spatial-anchor-name-not-found-msft -10000142001)
  ;; the spatial anchor name provided was not valid
  (:error-spatial-anchor-name-invalid-msft -10000142002)
  ;; anchor import from cloud or export from device failed.
  (:error-space-mapping-insufficient-fb -10000169000)
  ;; anchors were downloaded from the cloud but failed to be imported/aligned on the device.
  (:error-space-localization-failed-fb -10000169001)
  ;; timeout occurred while waiting for network request to complete.
  (:error-space-network-timeout-fb -10000169002)
  ;; the network request failed.
  (:error-space-network-request-failed-fb -10000169003)
  ;; cloud storage is required for this operation but is currently disabled.
  (:error-space-cloud-storage-disabled-fb -10000169004)
  ;; tracking optimization hint is already set for the domain.
  (:error-hint-already-set-qcom -10000306000))

;; Enums to track objects of various types
(defcenum object-type
  (:unknown 0)
  ;; xrinstance
  (:instance 1)
  ;; xrsession
  (:session 2)
  ;; xrswapchain
  (:swapchain 3)
  ;; xrspace
  (:space 4)
  ;; xractionset
  (:action-set 5)
  ;; xraction
  (:action 6)
  ;; xrdebugutilsmessengerext
  (:debug-utils-messenger-ext 10000019000)
  ;; xrspatialanchormsft
  (:spatial-anchor-msft 10000039000)
  ;; xrspatialgraphnodebindingmsft
  (:spatial-graph-node-binding-msft 10000049000)
  ;; xrhandtrackerext
  (:hand-tracker-ext 10000051000)
  ;; xrbodytrackerfb
  (:body-tracker-fb 10000076000)
  ;; xrsceneobservermsft
  (:scene-observer-msft 10000097000)
  ;; xrscenemsft
  (:scene-msft 10000097001)
  ;; xrfacialtrackerhtc
  (:facial-tracker-htc 10000104000)
  ;; xrfoveationprofilefb
  (:foveation-profile-fb 10000114000)
  ;; xrtrianglemeshfb
  (:triangle-mesh-fb 10000117000)
  ;; xrpassthroughfb
  (:passthrough-fb 10000118000)
  ;; xrpassthroughlayerfb
  (:passthrough-layer-fb 10000118002)
  ;; xrgeometryinstancefb
  (:geometry-instance-fb 10000118004)
  ;; xrspatialanchorstoreconnectionmsft
  (:spatial-anchor-store-connection-msft 10000142000)
  ;; xrfacetrackerfb
  (:face-tracker-fb 10000201000)
  ;; xreyetrackerfb
  (:eye-tracker-fb 10000202000)
  ;; xrspaceuserfb
  (:space-user-fb 10000241000)
  ;; xrpassthroughhtc
  (:passthrough-htc 10000317000))

;; Android Thread Types
(defcenum android-thread-type-khr
  (:application-main-khr 1)
  (:application-worker-khr 2)
  (:renderer-main-khr 3)
  (:renderer-worker-khr 4))

;; eye visibility selector
(defcenum eye-visibility
  ;; display in both eyes.
  (:both 0)
  ;; display in the left eye only.
  (:left 1)
  ;; display in the right eye only.
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
  (:unbounded-msft 10000038000)
  (:combined-eye-varjo 10000121000)
  (:local-floor-ext 10000426000))

(defcenum form-factor
  (:head-mounted-display 1)
  (:handheld-display 2))

(defcenum view-configuration-type
  (:primary-mono 1)
  (:primary-stereo 2)
  (:primary-quad-varjo 10000037000)
  (:secondary-mono-first-person-observer-msft 10000054000))

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
  ;; indicates that the performance settings or notification applies to cpu domain
  (:cpu-ext 1)
  ;; indicates that the performance settings or notification applies to gpu domain
  (:gpu-ext 2))

(defcenum perf-settings-sub-domain-ext
  ;; indicates that the performance notification originates from the compositing sub-domain
  (:compositing-ext 1)
  ;; indicates that the performance notification originates from the rendering sub-domain
  (:rendering-ext 2)
  ;; indicates that the performance notification originates from the thermal sub-domain
  (:thermal-ext 3))

(defcenum perf-settings-level-ext
  ;; performance settings hint used by the application to indicate that it enters a non-xr section (head-locked / static screen), during which power savings are to be prioritized
  (:power-savings-ext 0)
  ;; performance settings hint used by the application to indicate that it enters a low and stable complexity section, during which reducing power is more important than occasional late rendering frames
  (:sustained-low-ext 25)
  ;; performance settings hint used by the application to indicate that it enters a high or dynamic complexity section, during which the xr runtime strives for consistent xr compositing and frame rendering within a thermally sustainable range
  (:sustained-high-ext 50)
  ;; performance settings hint used by the application to indicate that the application enters a section with very high complexity, during which the xr runtime is allowed to step up beyond the thermally sustainable range
  (:boost-ext 75))

(defcenum perf-settings-notification-level-ext
  ;; notifies that the sub-domain has reached a level where no further actions other than currently applied are necessary
  (:level-normal-ext 0)
  ;; notifies that the sub-domain has reached an early warning level where the application should start proactive mitigation actions with the goal to return to the ename:xr_perf_notif_level_normal level
  (:level-warning-ext 25)
  ;; notifies that the sub-domain has reached a critical level with significant performance degradation. the application should take drastic mitigation action
  (:level-impaired-ext 75))

(defcenum visibility-mask-type-khr
  ;; exclusive mesh; indicates that which the viewer cannot see.
  (:hidden-triangle-mesh-khr 1)
  ;; inclusive mesh; indicates strictly that which the viewer can see.
  (:visible-triangle-mesh-khr 2)
  ;; line loop; traces the outline of the area the viewer can see.
  (:line-loop-khr 3))

(defcenum spatial-graph-node-type-msft
  (:static-msft 1)
  (:dynamic-msft 2))

(defcenum blend-factor-fb
  (:zero-fb 0)
  (:one-fb 1)
  (:src-alpha-fb 2)
  (:one-minus-src-alpha-fb 3)
  (:dst-alpha-fb 4)
  (:one-minus-dst-alpha-fb 5))

(defcenum space-component-type-fb
  ;; enables tracking the 6 dof pose of the slink:xrspace with flink:xrlocatespace.
  (:locatable-fb 0)
  ;; enables persistence operations: save and erase.
  (:storable-fb 1)
  ;; enables sharing of spatial entities.
  (:sharable-fb 2)
  ;; bounded 2d component.
  (:bounded-2d-fb 3)
  ;; bounded 3d component.
  (:bounded-3d-fb 4)
  ;; semantic labels component.
  (:semantic-labels-fb 5)
  ;; room layout component.
  (:room-layout-fb 6)
  ;; space container component.
  (:space-container-fb 7))

(defcenum winding-order-fb
  ;; winding order is unknown and the runtime cannot make any assumptions on the triangle orientation
  (:unknown-fb 0)
  ;; clockwise winding order
  (:cw-fb 1)
  ;; counter-clockwise winding order
  (:ccw-fb 2))

(defcenum passthrough-layer-purpose-fb
  ;; reconstruction passthrough (full screen environment)
  (:reconstruction-fb 0)
  ;; projected passthrough (using a custom surface)
  (:projected-fb 1)
  ;; passthrough layer purpose for keyboard hands presence.
  (:tracked-keyboard-hands-fb 10000203001)
  ;; passthrough layer purpose for keyboard hands presence with keyboard masked hand transitions (i.e passthrough hands rendered only when they are over the keyboard).
  (:tracked-keyboard-masked-hands-fb 10000203002))

(defcenum space-query-action-fb
  ;; tells the query to perform a load operation on any slink:xrspace returned by the query.
  (:load-fb 0))

(defcenum space-storage-location-fb
  ;; invalid storage location
  (:invalid-fb 0)
  ;; local device storage
  (:local-fb 1)
  ;; cloud storage
  (:cloud-fb 2))

(defcenum space-persistence-mode-fb
  ;; invalid storage persistence
  (:invalid-fb 0)
  ;; store slink:xrspace indefinitely, or until erased
  (:indefinite-fb 1))

(defcenum external-camera-attached-to-device-oculus
  ;; external camera is at a fixed point in local space
  (:none-oculus 0)
  ;; external camera is attached to the hmd
  (:hmd-oculus 1)
  ;; external camera is attached to a left touch controller
  (:ltouch-oculus 2)
  ;; external camera is attached to a right touch controller
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
  ;; the performance counter unit is hertz (hz).
  (:hertz-meta 4))

(defcenum facial-tracking-type-htc
  ;; specifies this handle will observe eye expressions, with values indexed by elink:xreyeexpressionhtc whose count is dlink:xr_facial_expression_eye_count_htc.
  (:eye-default-htc 1)
  ;; specifies this handle will observe lip expressions, with values indexed by elink:xrlipexpressionhtc whose count is dlink:xr_facial_expression_lip_count_htc.
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

(defcenum passthrough-form-htc
  ;; presents the passthrough with full of the entire screen.
  (:planar-htc 0)
  ;; presents the passthrough projecting onto a custom mesh.
  (:projected-htc 1))

(defcenum foveation-mode-htc
  ;; no foveation
  (:disable-htc 0)
  ;; apply system default setting with fixed clear fov and periphery quality.
  (:fixed-htc 1)
  ;; allow system to set foveation dynamically according realtime system metric or other extensions.
  (:dynamic-htc 2)
  ;; allow application to set foveation with desired clear fov, periphery quality, and focal center offset.
  (:custom-htc 3))

(defcenum foveation-level-htc
  ;; no foveation
  (:none-htc 0)
  ;; light periphery pixel density drop and lower performance gain.
  (:low-htc 1)
  ;; medium periphery pixel density drop and medium performance gain
  (:medium-htc 2)
  ;; heavy periphery pixel density drop and higher performance gain
  (:igh-htc 3))

(defcenum local-dimming-mode-meta
  ;; local dimming is turned off by default for the current submitted frame. this is the same as not chaining elink:xrlocaldimmingmodemeta.
  (:off-meta 0)
  ;; local dimming is turned on for the current submitted frame.
  (:on-meta 1))

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
  ;; indicates that the created slink:xrfacetrackerfb tracks the set of blend shapes described by elink:xrfaceexpressionfb enum, i.e. the flink:xrgetfaceexpressionweightsfb function returns an array of blend shapes with the count of ename:xr_face_expression_count_fb and can: be indexed using elink:xrfaceexpressionfb.
  (:default-fb 0))

(defcenum face-confidence-fb
  (:lower-face-fb 0)
  (:upper-face-fb 1)
  (:count-fb 2))

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
  ;; indicates that the created slink:xrbodytrackerfb tracks the set of body joints described by elink:xrbodyjointfb enum, i.e. the flink:xrlocatebodyjointsfb function returns an array of joint locations with the count of ename:xr_body_joint_count_fb and can be indexed using elink:xrbodyjointfb.
  (:default-fb 0))

(defcenum eye-position-fb
  ;; specifies the position of the left eye.
  (:left-fb 0)
  ;; specifies the position of the right eye.
  (:right-fb 1)
  (:count-fb 2))

(defcenum tracking-optimization-settings-domain-qcom
  ;; setting applies to all qcom tracking extensions.
  (:all-qcom 1))

(defcenum tracking-optimization-settings-hint-qcom
  ;; used by the application to indicate that it does not have a preference to optimize for. the run-time is understood to choose a balanced approach.
  (:none-qcom 0)
  ;; used by the application to indicate that it prefers tracking to be optimized for long range, possibly at the expense of competing interests.
  (:long-range-priorization-qcom 1)
  ;; used by the application to indicate that it prefers tracking to be optimized for close range, possibly at the expense of competing interests.
  (:close-range-priorization-qcom 2)
  ;; used by the application to indicate that it prefers tracking to be optimized for low power consumption, possibly at the expense of competing interests.
  (:low-power-priorization-qcom 3)
  ;; used by the application to indicate that it prefers tracking to be optimized for increased tracking performance, possibly at the cost of increased power consumption.
  (:high-power-priorization-qcom 4))

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

(defconstant +max-api-layer-name-size+ 256)

(defconstant +max-api-layer-description-size+ 256)

(defconstant +max-extension-name-size+ 128)

(defconstant +max-application-name-size+ 128)

(defconstant +max-engine-name-size+ 128)

(defconstant +max-runtime-name-size+ 128)

(defconstant +max-system-name-size+ 256)

(defconstant +max-action-set-name-size+ 64)

(defconstant +max-localized-action-set-name-size+ 128)

(defconstant +max-action-name-size+ 64)

(defconstant +max-localized-action-name-size+ 128)

(defconstant +guid-size-msft+ 16)

(defconstant +eye-position-count-fb+ 2)

(defconstant +max-controller-model-node-name-size-msft+ 64)

(defconstant +foveation-center-size-meta+ 2)

(defconstant +hand-tracking-capsule-point-count-fb+ 2)

(defconstant +hand-tracking-capsule-count-fb+ 19)

(defconstant +max-render-model-name-size-fb+ 64)

(defconstant +max-keyboard-tracking-name-size-fb+ 128)

(defconstant +passthrough-color-map-mono-size-fb+ 256)

(defconstant +max-spatial-anchor-name-size-msft+ 256)

(defconstant +uuid-size-ext+ 16)

(defconstant +max-external-camera-name-size-oculus+ 32)

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

;; XrBaseInStructure and XrBaseOutStructure use "struct" in their member definitions
;;         because they are recursive structures and this is easier than modifying the tooling
;;         to output forward declarations.
(defcstruct base-in-structure
  (type structure-type)
  (next (:pointer (:pointer (:struct base-in-structure)))))

(defcstruct base-out-structure
  (type structure-type)
  (next (:pointer (:pointer (:struct base-out-structure)))))

(defcstruct api-layer-properties
  (type structure-type) ;; = type-api-layer-properties
  (next (:pointer (:pointer :void)))
  (layer-name :char :count +max-api-layer-name-size+)
  (spec-version version)
  (layer-version :uint32)
  (description :char :count +max-api-layer-description-size+))

(defcstruct extension-properties
  (type structure-type) ;; = type-extension-properties
  (next (:pointer (:pointer :void)))
  (extension-name :char :count +max-extension-name-size+)
  (extension-version :uint32))

(defcstruct application-info
  (application-name :char :count +max-application-name-size+)
  (application-version :uint32)
  (engine-name :char :count +max-engine-name-size+)
  (engine-version :uint32)
  (api-version version))

(defcstruct instance-create-info
  (type structure-type) ;; = type-instance-create-info
  (next (:pointer (:pointer :void)))
  (create-flags instance-create-flags) ;; optional
  (application-info (:struct application-info))
  (enabled-api-layer-count :uint32) ;; optional
  (enabled-api-layer-names (:pointer (:pointer (:pointer (:pointer :string))))) ;; count enabled-api-layer-count
  (enabled-extension-count :uint32) ;; optional
  (enabled-extension-names (:pointer (:pointer (:pointer (:pointer :string))))) ;; count enabled-extension-count
)

(defcstruct instance-properties
  (type structure-type) ;; = type-instance-properties
  (next (:pointer (:pointer :void)))
  (runtime-version version)
  (runtime-name :char :count +max-runtime-name-size+))

(defcstruct system-get-info
  (type structure-type) ;; = type-system-get-info
  (next (:pointer (:pointer :void)))
  (form-factor form-factor))

(defcstruct system-tracking-properties
  (orientation-tracking bool-32)
  (position-tracking bool-32))

(defcstruct system-graphics-properties
  (max-swapchain-image-height :uint32)
  (max-swapchain-image-width :uint32)
  (max-layer-count :uint32))

(defcstruct system-properties
  (type structure-type) ;; = type-system-properties
  (next (:pointer (:pointer :void)))
  (system-id system-id)
  (vendor-id :uint32)
  (system-name :char :count +max-system-name-size+)
  (graphics-properties (:struct system-graphics-properties))
  (tracking-properties (:struct system-tracking-properties)))

(defcstruct graphics-binding-opengl-win-32-khr
  (type structure-type) ;; = type-graphics-binding-opengl-win32-khr
  (next (:pointer (:pointer :void)))
  (hdc hdc)
  (hglrc hglrc))

(defcstruct graphics-binding-opengl-xlib-khr
  (type structure-type) ;; = type-graphics-binding-opengl-xlib-khr
  (next (:pointer (:pointer :void)))
  (x-display (:pointer (:pointer display)))
  (visualid :uint32)
  (glx-fbconfig glx-fb-config)
  (glx-drawable glx-drawable)
  (glx-context glx-context))

(defcstruct graphics-binding-opengl-xcb-khr
  (type structure-type) ;; = type-graphics-binding-opengl-xcb-khr
  (next (:pointer (:pointer :void)))
  (connection (:pointer (:pointer xcb-connection-t)))
  (screen-number :uint32)
  (fbconfigid xcb-glx-fbconfig-t)
  (visualid xcb-visualid-t)
  (glx-drawable xcb-glx-drawable-t)
  (glx-context xcb-glx-context-t))

(defcstruct graphics-binding-opengl-wayland-khr
  (type structure-type) ;; = type-graphics-binding-opengl-wayland-khr
  (next (:pointer (:pointer :void)))
  (display (:pointer (:pointer wl-display))))

(defcstruct graphics-binding-d3d11-khr
  (type structure-type) ;; = type-graphics-binding-d3d11-khr
  (next (:pointer (:pointer :void)))
  (device (:pointer (:pointer i-d3d11-device))))

(defcstruct graphics-binding-d3d12-khr
  (type structure-type) ;; = type-graphics-binding-d3d12-khr
  (next (:pointer (:pointer :void)))
  (device (:pointer (:pointer i-d3d12-device)))
  (queue (:pointer (:pointer i-d3d12-command-queue))))

(defcstruct graphics-binding-opengl-esandroid-khr
  (type structure-type) ;; = type-graphics-binding-opengl-es-android-khr
  (next (:pointer (:pointer :void)))
  (display egl-display)
  (config egl-config)
  (context egl-context))

(defcstruct graphics-binding-vulkan-khr
  (type structure-type) ;; = type-graphics-binding-vulkan-khr
  (next (:pointer (:pointer :void)))
  (instance vk-image)
  (physical-device vk-physical-device)
  (device vk-device)
  (queue-family-index :uint32)
  (queue-index :uint32))

(defcstruct session-create-info
  (type structure-type) ;; = type-session-create-info
  (next (:pointer (:pointer :void)))
  (create-flags session-create-flags) ;; optional
  (system-id system-id))

(defcstruct session-begin-info
  (type structure-type) ;; = type-session-begin-info
  (next (:pointer (:pointer :void)))
  (primary-view-configuration-type view-configuration-type))

(defcstruct swapchain-create-info
  (type structure-type) ;; = type-swapchain-create-info
  (next (:pointer (:pointer :void)))
  (create-flags swapchain-create-flags) ;; optional
  (usage-flags swapchain-usage-flags) ;; optional
  (format :int64)
  (sample-count :uint32)
  (width :uint32)
  (height :uint32)
  (face-count :uint32)
  (array-size :uint32)
  (mip-count :uint32))

(defcstruct swapchain-image-base-header
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct swapchain-image-opengl-khr
  (type structure-type) ;; = type-swapchain-image-opengl-khr
  (next (:pointer (:pointer :void)))
  (image :uint32))

(defcstruct swapchain-image-opengl-es-khr
  (type structure-type) ;; = type-swapchain-image-opengl-es-khr
  (next (:pointer (:pointer :void)))
  (image :uint32))

(defcstruct swapchain-image-vulkan-khr
  (type structure-type) ;; = type-swapchain-image-vulkan-khr
  (next (:pointer (:pointer :void)))
  (image vk-image))

(defcstruct swapchain-image-d3d11-khr
  (type structure-type) ;; = type-swapchain-image-d3d11-khr
  (next (:pointer (:pointer :void)))
  (texture (:pointer (:pointer i-d3d11-texture-2d))))

(defcstruct swapchain-image-d3d12-khr
  (type structure-type) ;; = type-swapchain-image-d3d12-khr
  (next (:pointer (:pointer :void)))
  (texture (:pointer (:pointer i-d3d12-resource))))

(defcstruct swapchain-image-acquire-info
  (type structure-type) ;; = type-swapchain-image-acquire-info
  (next (:pointer (:pointer :void))))

(defcstruct swapchain-image-wait-info
  (type structure-type) ;; = type-swapchain-image-wait-info
  (next (:pointer (:pointer :void)))
  (timeout duration))

(defcstruct swapchain-image-release-info
  (type structure-type) ;; = type-swapchain-image-release-info
  (next (:pointer (:pointer :void))))

(defcstruct reference-space-create-info
  (type structure-type) ;; = type-reference-space-create-info
  (next (:pointer (:pointer :void)))
  (reference-space-type reference-space-type)
  (pose-in-reference-space (:struct pose-f)))

(defcstruct action-space-create-info
  (type structure-type) ;; = type-action-space-create-info
  (next (:pointer (:pointer :void)))
  (action action)
  (subaction-path path) ;; optional
  (pose-in-action-space (:struct pose-f)))

(defcstruct space-location
  (type structure-type) ;; = type-space-location
  (next (:pointer (:pointer :void)))
  (location-flags space-location-flags) ;; optional
  (pose (:struct pose-f)))

(defcstruct space-velocity
  (type structure-type) ;; = type-space-velocity
  (next (:pointer (:pointer :void)))
  (velocity-flags space-velocity-flags) ;; optional
  (linear-velocity (:struct vector-3f))
  (angular-velocity (:struct vector-3f)))

(defcstruct fov-f
  (angle-left :float)
  (angle-right :float)
  (angle-up :float)
  (angle-down :float))

(defcstruct view
  (type structure-type) ;; = type-view
  (next (:pointer (:pointer :void)))
  (pose (:struct pose-f))
  (fov (:struct fov-f)))

(defcstruct view-locate-info
  (type structure-type) ;; = type-view-locate-info
  (next (:pointer (:pointer :void)))
  (view-configuration-type view-configuration-type)
  (display-time time)
  (space space))

(defcstruct view-state
  (type structure-type) ;; = type-view-state
  (next (:pointer (:pointer :void)))
  (view-state-flags view-state-flags) ;; optional
)

(defcstruct view-configuration-view
  (type structure-type) ;; = type-view-configuration-view
  (next (:pointer (:pointer :void)))
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
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space))

(defcstruct composition-layer-projection-view
  (type structure-type) ;; = type-composition-layer-projection-view
  (next (:pointer (:pointer :void)))
  (pose (:struct pose-f))
  (fov (:struct fov-f))
  (sub-image (:struct swapchain-sub-image)))

(defcstruct composition-layer-projection
  (type structure-type) ;; = type-composition-layer-projection
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (view-count :uint32)
  (views (:pointer (:pointer (:struct composition-layer-projection-view)))) ;; count view-count
)

(defcstruct composition-layer-quad
  (type structure-type) ;; = type-composition-layer-quad
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (size (:struct extent-2d-f)))

(defcstruct composition-layer-cylinder-khr
  (type structure-type) ;; = type-composition-layer-cylinder-khr
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (radius :float)
  (central-angle :float)
  (aspect-ratio :float))

(defcstruct composition-layer-cube-khr
  (type structure-type) ;; = type-composition-layer-cube-khr
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (swapchain swapchain)
  (image-array-index :uint32)
  (orientation (:struct quaternion-f)))

(defcstruct composition-layer-equirect-khr
  (type structure-type) ;; = type-composition-layer-equirect-khr
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (radius :float)
  (scale (:struct vector-2f))
  (bias (:struct vector-2f)))

(defcstruct composition-layer-depth-info-khr
  (type structure-type) ;; = type-composition-layer-depth-info-khr
  (next (:pointer (:pointer :void)))
  (sub-image (:struct swapchain-sub-image))
  (min-depth :float)
  (max-depth :float)
  (near-z :float)
  (far-z :float))

(defcstruct frame-begin-info
  (type structure-type) ;; = type-frame-begin-info
  (next (:pointer (:pointer :void))))

(defcstruct frame-end-info
  (type structure-type) ;; = type-frame-end-info
  (next (:pointer (:pointer :void)))
  (display-time time)
  (environment-blend-mode environment-blend-mode)
  (layer-count :uint32) ;; optional
  (layers (:pointer
           (:pointer
            (:pointer (:pointer (:struct composition-layer-base-header)))))) ;; count layer-count, optional
)

(defcstruct frame-wait-info
  (type structure-type) ;; = type-frame-wait-info
  (next (:pointer (:pointer :void))))

(defcstruct frame-state
  (type structure-type) ;; = type-frame-state
  (next (:pointer (:pointer :void)))
  (predicted-display-time time)
  (predicted-display-period duration)
  (should-render bool-32))

(defcstruct haptic-base-header
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct haptic-vibration
  (type structure-type) ;; = type-haptic-vibration
  (next (:pointer (:pointer :void)))
  (duration duration)
  (frequency :float) ;; optional
  (amplitude :float))

(defcstruct event-data-base-header
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct event-data-buffer
  (type structure-type) ;; = type-event-data-buffer
  (next (:pointer (:pointer :void)))
  (varying :uint8 :count 4000))

(defcstruct event-data-events-lost
  (type structure-type) ;; = type-event-data-events-lost
  (next (:pointer (:pointer :void)))
  (lost-event-count :uint32))

(defcstruct event-data-instance-loss-pending
  (type structure-type) ;; = type-event-data-instance-loss-pending
  (next (:pointer (:pointer :void)))
  (loss-time time))

(defcstruct event-data-session-state-changed
  (type structure-type) ;; = type-event-data-session-state-changed
  (next (:pointer (:pointer :void)))
  (session session)
  (state session-state)
  (time time))

(defcstruct event-data-reference-space-change-pending
  (type structure-type) ;; = type-event-data-reference-space-change-pending
  (next (:pointer (:pointer :void)))
  (session session)
  (reference-space-type reference-space-type)
  (change-time time)
  (pose-valid bool-32)
  (pose-in-previous-space (:struct pose-f)))

(defcstruct event-data-perf-settings-ext
  (type structure-type) ;; = type-event-data-perf-settings-ext
  (next (:pointer (:pointer :void)))
  (domain perf-settings-domain-ext)
  (sub-domain perf-settings-sub-domain-ext)
  (from-level perf-settings-notification-level-ext)
  (to-level perf-settings-notification-level-ext))

(defcstruct event-data-visibility-mask-changed-khr
  (type structure-type) ;; = type-event-data-visibility-mask-changed-khr
  (next (:pointer (:pointer :void)))
  (session session)
  (view-configuration-type view-configuration-type)
  (view-index :uint32))

(defcstruct view-configuration-properties
  (type structure-type) ;; = type-view-configuration-properties
  (next (:pointer (:pointer :void)))
  (view-configuration-type view-configuration-type)
  (fov-mutable bool-32))

(defcstruct action-state-boolean
  (type structure-type) ;; = type-action-state-boolean
  (next (:pointer (:pointer :void)))
  (current-state bool-32)
  (changed-since-last-sync bool-32)
  (last-change-time time)
  (is-active bool-32))

(defcstruct action-state-float
  (type structure-type) ;; = type-action-state-float
  (next (:pointer (:pointer :void)))
  (current-state :float)
  (changed-since-last-sync bool-32)
  (last-change-time time)
  (is-active bool-32))

(defcstruct action-state-vector-2f
  (type structure-type) ;; = type-action-state-vector2f
  (next (:pointer (:pointer :void)))
  (current-state (:struct vector-2f))
  (changed-since-last-sync bool-32)
  (last-change-time time)
  (is-active bool-32))

(defcstruct action-state-pose
  (type structure-type) ;; = type-action-state-pose
  (next (:pointer (:pointer :void)))
  (is-active bool-32))

(defcstruct action-state-get-info
  (type structure-type) ;; = type-action-state-get-info
  (next (:pointer (:pointer :void)))
  (action action)
  (subaction-path path) ;; optional
)

(defcstruct haptic-action-info
  (type structure-type) ;; = type-haptic-action-info
  (next (:pointer (:pointer :void)))
  (action action)
  (subaction-path path) ;; optional
)

(defcstruct action-set-create-info
  (type structure-type) ;; = type-action-set-create-info
  (next (:pointer (:pointer :void)))
  (action-set-name :char :count +max-action-set-name-size+)
  (localized-action-set-name :char :count +max-localized-action-set-name-size+)
  (priority :uint32))

(defcstruct action-suggested-binding
  (action action)
  (binding path))

(defcstruct interaction-profile-suggested-binding
  (type structure-type) ;; = type-interaction-profile-suggested-binding
  (next (:pointer (:pointer :void)))
  (interaction-profile path)
  (count-suggested-bindings :uint32)
  (suggested-bindings (:pointer (:pointer (:struct action-suggested-binding)))) ;; count count-suggested-bindings
)

(defcstruct active-action-set
  (action-set action-set)
  (subaction-path path))

(defcstruct session-action-sets-attach-info
  (type structure-type) ;; = type-session-action-sets-attach-info
  (next (:pointer (:pointer :void)))
  (count-action-sets :uint32)
  (action-sets (:pointer (:pointer action-set))) ;; count count-action-sets
)

(defcstruct actions-sync-info
  (type structure-type) ;; = type-actions-sync-info
  (next (:pointer (:pointer :void)))
  (count-active-action-sets :uint32) ;; optional
  (active-action-sets (:pointer (:pointer (:struct active-action-set)))) ;; count count-active-action-sets, optional
)

(defcstruct bound-sources-for-action-enumerate-info
  (type structure-type) ;; = type-bound-sources-for-action-enumerate-info
  (next (:pointer (:pointer :void)))
  (action action))

(defcstruct input-source-localized-name-get-info
  (type structure-type) ;; = type-input-source-localized-name-get-info
  (next (:pointer (:pointer :void)))
  (source-path path)
  (which-components input-source-localized-name-flags))

(defcstruct event-data-interaction-profile-changed
  (type structure-type) ;; = type-event-data-interaction-profile-changed
  (next (:pointer (:pointer :void)))
  (session session))

(defcstruct interaction-profile-state
  (type structure-type) ;; = type-interaction-profile-state
  (next (:pointer (:pointer :void)))
  (interaction-profile path))

(defcstruct action-create-info
  (type structure-type) ;; = type-action-create-info
  (next (:pointer (:pointer :void)))
  (action-name :char :count +max-action-name-size+)
  (action-type action-type)
  (count-subaction-paths :uint32) ;; optional
  (subaction-paths (:pointer (:pointer path))) ;; count count-subaction-paths, optional
  (localized-action-name :char :count +max-localized-action-name-size+))

(defcstruct instance-create-info-android-khr
  (type structure-type) ;; = type-instance-create-info-android-khr
  (next (:pointer (:pointer :void)))
  (application-vm (:pointer (:pointer :void)))
  (application-activity (:pointer (:pointer :void))))

(defcstruct vulkan-swapchain-format-list-create-info-khr
  (type structure-type) ;; = type-vulkan-swapchain-format-list-create-info-khr
  (next (:pointer (:pointer :void)))
  (view-format-count :uint32) ;; optional
  (view-formats (:pointer (:pointer vk-format))) ;; count view-format-count
)

(defcstruct debug-utils-object-name-info-ext
  (type structure-type) ;; = type-debug-utils-object-name-info-ext
  (next (:pointer (:pointer :void)))
  (object-type object-type)
  (object-handle :uint64)
  (object-name (:pointer (:pointer :string))) ;; optional
)

(defcstruct debug-utils-label-ext
  (type structure-type) ;; = type-debug-utils-label-ext
  (next (:pointer (:pointer :void)))
  (label-name (:pointer (:pointer :string))))

(defcstruct debug-utils-messenger-callback-data-ext
  (type structure-type) ;; = type-debug-utils-messenger-callback-data-ext
  (next (:pointer (:pointer :void)))
  (message-id (:pointer (:pointer :string)))
  (function-name (:pointer (:pointer :string)))
  (message (:pointer (:pointer :string)))
  (object-count :uint32) ;; optional
  (objects (:pointer (:pointer (:struct debug-utils-object-name-info-ext)))) ;; count object-count, noautovalidity optional
  (session-label-count :uint32) ;; optional
  (session-labels (:pointer (:pointer (:struct debug-utils-label-ext)))) ;; count session-label-count, noautovalidity optional
)

(defcstruct debug-utils-messenger-create-info-ext
  (type structure-type) ;; = type-debug-utils-messenger-create-info-ext
  (next (:pointer (:pointer :void)))
  (message-severities debug-utils-message-severity-flags-ext)
  (message-types debug-utils-message-type-flags-ext)
  (user-callback :pointer)
  (user-data (:pointer (:pointer :void))) ;; optional
)

(defcstruct visibility-mask-khr
  (type structure-type) ;; = type-visibility-mask-khr
  (next (:pointer (:pointer :void)))
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32) ;; optional
  (vertices (:pointer (:pointer (:struct vector-2f)))) ;; count vertex-capacity-input, optional
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32) ;; optional
  (indices (:pointer (:pointer :uint32))) ;; count index-capacity-input, optional
)

(defcstruct graphics-requirements-opengl-khr
  (type structure-type) ;; = type-graphics-requirements-opengl-khr
  (next (:pointer (:pointer :void)))
  (min-api-version-supported version)
  (max-api-version-supported version))

(defcstruct graphics-requirements-opengl-es-khr
  (type structure-type) ;; = type-graphics-requirements-opengl-es-khr
  (next (:pointer (:pointer :void)))
  (min-api-version-supported version)
  (max-api-version-supported version))

(defcstruct graphics-requirements-vulkan-khr
  (type structure-type) ;; = type-graphics-requirements-vulkan-khr
  (next (:pointer (:pointer :void)))
  (min-api-version-supported version)
  (max-api-version-supported version))

(defcstruct graphics-requirements-d3d11-khr
  (type structure-type) ;; = type-graphics-requirements-d3d11-khr
  (next (:pointer (:pointer :void)))
  (adapter-luid luid)
  (min-feature-level d3d-feature-level))

(defcstruct graphics-requirements-d3d12-khr
  (type structure-type) ;; = type-graphics-requirements-d3d12-khr
  (next (:pointer (:pointer :void)))
  (adapter-luid luid)
  (min-feature-level d3d-feature-level))

(defcstruct vulkan-instance-create-info-khr
  (type structure-type) ;; = type-vulkan-instance-create-info-khr
  (next (:pointer (:pointer :void)))
  (system-id system-id)
  (create-flags vulkan-instance-create-flags-khr) ;; optional
  (pfn-get-instance-proc-addr vk-get-device-instance-proc-addr)
  (vulkan-create-info (:pointer (:pointer vk-instance-create-info)))
  (vulkan-allocator (:pointer (:pointer vk-allocation-callbacks))) ;; optional
)

(defcstruct vulkan-device-create-info-khr
  (type structure-type) ;; = type-vulkan-device-create-info-khr
  (next (:pointer (:pointer :void)))
  (system-id system-id)
  (create-flags vulkan-device-create-flags-khr) ;; optional
  (pfn-get-instance-proc-addr vk-get-device-instance-proc-addr)
  (vulkan-physical-device vk-physical-device)
  (vulkan-create-info (:pointer (:pointer vk-device-create-info)))
  (vulkan-allocator (:pointer (:pointer vk-allocation-callbacks))) ;; optional
)

(defcstruct graphics-binding-vulkan-2-khr)

(defcstruct vulkan-graphics-device-get-info-khr
  (type structure-type) ;; = type-vulkan-graphics-device-get-info-khr
  (next (:pointer (:pointer :void)))
  (system-id system-id)
  (vulkan-instance vk-image))

(defcstruct swapchain-image-vulkan-2-khr)

(defcstruct graphics-requirements-vulkan-2-khr)

(defcstruct vulkan-swapchain-create-info-meta
  (type structure-type) ;; = type-vulkan-swapchain-create-info-meta
  (next (:pointer (:pointer :void)))
  (additional-create-flags vk-image-create-flags)
  (additional-usage-flags vk-image-usage-flags))

(defcstruct session-create-info-overlay-extx
  (type structure-type) ;; = type-session-create-info-overlay-extx
  (next (:pointer (:pointer :void)))
  (create-flags overlay-session-create-flags-extx)
  (session-layers-placement :uint32))

(defcstruct event-data-main-session-visibility-changed-extx
  (type structure-type) ;; = type-event-data-main-session-visibility-changed-extx
  (next (:pointer (:pointer :void)))
  (visible bool-32)
  (flags overlay-main-session-flags-extx))

(defcstruct event-data-display-refresh-rate-changed-fb
  (type structure-type) ;; = type-event-data-display-refresh-rate-changed-fb
  (next (:pointer (:pointer :void)))
  (from-display-refresh-rate :float)
  (to-display-refresh-rate :float))

(defcstruct view-configuration-depth-range-ext
  (type structure-type) ;; = type-view-configuration-depth-range-ext
  (next (:pointer (:pointer :void)))
  (recommended-near-z :float)
  (min-near-z :float)
  (recommended-far-z :float)
  (max-far-z :float))

(defcstruct view-configuration-view-fov-epic
  (type structure-type) ;; = type-view-configuration-view-fov-epic
  (next (:pointer (:pointer :void)))
  (recommended-fov (:struct fov-f))
  (max-mutable-fov (:struct fov-f)))

(defcstruct interaction-profile-dpad-binding-ext
  (type structure-type) ;; = type-interaction-profile-dpad-binding-ext
  (next (:pointer (:pointer :void)))
  (binding path)
  (action-set action-set)
  (force-threshold :float)
  (force-threshold-released :float)
  (center-region :float)
  (wedge-angle :float)
  (is-sticky bool-32)
  (on-haptic (:pointer (:pointer (:struct haptic-base-header)))) ;; optional
  (off-haptic (:pointer (:pointer (:struct haptic-base-header)))) ;; optional
)

(defcstruct interaction-profile-analog-threshold-valve
  (type structure-type) ;; = type-interaction-profile-analog-threshold-valve
  (next (:pointer (:pointer :void)))
  (action action)
  (binding path)
  (on-threshold :float)
  (off-threshold :float)
  (on-haptic (:pointer (:pointer (:struct haptic-base-header)))) ;; optional
  (off-haptic (:pointer (:pointer (:struct haptic-base-header)))) ;; optional
)

(defcstruct binding-modification-base-header-khr
  (type structure-type) ;; noautovalidity
  (next (:pointer (:pointer :void))))

(defcstruct binding-modifications-khr
  (type structure-type) ;; = type-binding-modifications-khr
  (next (:pointer (:pointer :void)))
  (binding-modification-count :uint32) ;; optional
  (binding-modifications (:pointer
                          (:pointer
                           (:pointer
                            (:pointer
                             (:struct binding-modification-base-header-khr)))))) ;; count binding-modification-count, optional
)

;;; typedef void (XRAPI_PTR *PFN_xrVoidFunction)(void);
;;;
;;; typedef XrBool32 (XRAPI_PTR *PFN_xrDebugUtilsMessengerCallbackEXT)(
;;;            XrDebugUtilsMessageSeverityFlagsEXT              messageSeverity,
;;;            XrDebugUtilsMessageTypeFlagsEXT                  messageTypes,
;;;            const XrDebugUtilsMessengerCallbackDataEXT*      callbackData,
;;;            void*                                            userData);
;;;
(defcstruct system-eye-gaze-interaction-properties-ext
  (type structure-type) ;; = type-system-eye-gaze-interaction-properties-ext
  (next (:pointer (:pointer :void)))
  (supports-eye-gaze-interaction bool-32))

(defcstruct eye-gaze-sample-time-ext
  (type structure-type) ;; = type-eye-gaze-sample-time-ext
  (next (:pointer (:pointer :void)))
  (time time))

(defcstruct spatial-anchor-create-info-msft
  (type structure-type) ;; = type-spatial-anchor-create-info-msft
  (next (:pointer (:pointer :void)))
  (space space)
  (pose (:struct pose-f))
  (time time))

(defcstruct spatial-anchor-space-create-info-msft
  (type structure-type) ;; = type-spatial-anchor-space-create-info-msft
  (next (:pointer (:pointer :void)))
  (anchor spatial-anchor-msft)
  (pose-in-anchor-space (:struct pose-f)))

(defcstruct composition-layer-image-layout-fb
  (type structure-type) ;; = type-composition-layer-image-layout-fb
  (next (:pointer (:pointer :void)))
  (flags composition-layer-image-layout-flags-fb) ;; optional
)

(defcstruct composition-layer-alpha-blend-fb
  (type structure-type) ;; = type-composition-layer-alpha-blend-fb
  (next (:pointer (:pointer :void)))
  (src-factor-color blend-factor-fb)
  (dst-factor-color blend-factor-fb)
  (src-factor-alpha blend-factor-fb)
  (dst-factor-alpha blend-factor-fb))

(defcstruct graphics-binding-eglmndx
  (type structure-type) ;; = type-graphics-binding-egl-mndx
  (next (:pointer (:pointer :void)))
  (get-proc-address pfn-egl-get-proc-address-proc)
  (display egl-display)
  (config egl-config)
  (context egl-context))

(defctype spatial-graph-node-binding-msft xr-handle)
(defcstruct spatial-graph-node-space-create-info-msft
  (type structure-type) ;; = type-spatial-graph-node-space-create-info-msft
  (next (:pointer (:pointer :void)))
  (node-type spatial-graph-node-type-msft)
  (node-id :uint8 :count +guid-size-msft+)
  (pose (:struct pose-f)))

(defcstruct spatial-graph-static-node-binding-create-info-msft
  (type structure-type) ;; = type-spatial-graph-static-node-binding-create-info-msft
  (next (:pointer (:pointer :void)))
  (space space)
  (pose-in-space (:struct pose-f))
  (time time))

(defcstruct spatial-graph-node-binding-properties-get-info-msft
  (type structure-type) ;; = type-spatial-graph-node-binding-properties-get-info-msft
  (next (:pointer (:pointer :void))))

(defcstruct spatial-graph-node-binding-properties-msft
  (type structure-type) ;; = type-spatial-graph-node-binding-properties-msft
  (next (:pointer (:pointer :void)))
  (node-id :uint8 :count +guid-size-msft+)
  (pose-in-node-space (:struct pose-f)))

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
  (:hand-with-forearm-ultraleap 10000149000))

(defcstruct system-hand-tracking-properties-ext
  (type structure-type) ;; = type-system-hand-tracking-properties-ext
  (next (:pointer (:pointer :void)))
  (supports-hand-tracking bool-32))

(defcstruct hand-tracker-create-info-ext
  (type structure-type) ;; = type-hand-tracker-create-info-ext
  (next (:pointer (:pointer :void)))
  (hand hand-ext)
  (hand-joint-set hand-joint-set-ext))

(defcstruct hand-joints-locate-info-ext
  (type structure-type) ;; = type-hand-joints-locate-info-ext
  (next (:pointer (:pointer :void)))
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
  (type structure-type) ;; = type-hand-joint-locations-ext
  (next (:pointer (:pointer :void)))
  (is-active bool-32)
  (joint-count :uint32)
  (joint-locations (:pointer (:pointer (:struct hand-joint-location-ext)))) ;; count joint-count
)

(defcstruct hand-joint-velocities-ext
  (type structure-type) ;; = type-hand-joint-velocities-ext
  (next (:pointer (:pointer :void)))
  (joint-count :uint32)
  (joint-velocities (:pointer (:pointer (:struct hand-joint-velocity-ext)))) ;; count joint-count
)

(defcstruct system-face-tracking-properties-fb
  (type structure-type) ;; = type-system-face-tracking-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-face-tracking bool-32))

(defcstruct face-tracker-create-info-fb
  (type structure-type) ;; = type-face-tracker-create-info-fb
  (next (:pointer (:pointer :void)))
  (face-expression-set face-expression-set-fb))

(defcstruct face-expression-info-fb
  (type structure-type) ;; = type-face-expression-info-fb
  (next (:pointer (:pointer :void)))
  (time time))

(defcstruct face-expression-status-fb
  (is-valid bool-32)
  (is-eye-following-blendshapes-valid bool-32))

(defcstruct face-expression-weights-fb
  (type structure-type) ;; = type-face-expression-weights-fb
  (next (:pointer (:pointer :void)))
  (weight-count :uint32)
  (weights (:pointer (:pointer :float))) ;; count weight-count
  (confidence-count :uint32)
  (confidences (:pointer (:pointer :float))) ;; count confidence-count
  (status (:struct face-expression-status-fb))
  (time time))

(defcstruct system-body-tracking-properties-fb
  (type structure-type) ;; = type-system-body-tracking-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-body-tracking bool-32))

(defcstruct body-tracker-create-info-fb
  (type structure-type) ;; = type-body-tracker-create-info-fb
  (next (:pointer (:pointer :void)))
  (body-joint-set body-joint-set-fb))

(defcstruct body-skeleton-joint-fb
  (joint :int32)
  (parent-joint :int32)
  (pose (:struct pose-f)))

(defcstruct body-skeleton-fb
  (type structure-type) ;; = type-body-skeleton-fb
  (next (:pointer (:pointer :void)))
  (joint-count :uint32)
  (joints (:pointer (:pointer (:struct body-skeleton-joint-fb)))) ;; count joint-count
)

(defcstruct body-joints-locate-info-fb
  (type structure-type) ;; = type-body-joints-locate-info-fb
  (next (:pointer (:pointer :void)))
  (base-space space)
  (time time))

(defcstruct body-joint-location-fb
  (location-flags space-location-flags)
  (pose (:struct pose-f)))

(defcstruct body-joint-locations-fb
  (type structure-type) ;; = type-body-joint-locations-fb
  (next (:pointer (:pointer :void)))
  (is-active bool-32)
  (confidence :float)
  (joint-count :uint32)
  (joint-locations (:pointer (:pointer (:struct body-joint-location-fb)))) ;; count joint-count
  (skeleton-changed-count :uint32)
  (time time))

(defcstruct system-eye-tracking-properties-fb
  (type structure-type) ;; = type-system-eye-tracking-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-eye-tracking bool-32))

(defcstruct eye-tracker-create-info-fb
  (type structure-type) ;; = type-eye-tracker-create-info-fb
  (next (:pointer (:pointer :void))))

(defcstruct eye-gazes-info-fb
  (type structure-type) ;; = type-eye-gazes-info-fb
  (next (:pointer (:pointer :void)))
  (base-space space)
  (time time))

(defcstruct eye-gaze-fb
  (is-valid bool-32)
  (gaze-pose (:struct pose-f))
  (gaze-confidence :float))

(defcstruct eye-gazes-fb
  (type structure-type) ;; = type-eye-gazes-fb
  (next (:pointer (:pointer :void)))
  (gaze (:struct eye-gaze-fb) :count +eye-position-count-fb+)
  (time time))

(defcenum hand-joints-motion-range-ext
  (:unobstructed-ext 1)
  (:conforming-to-controller-ext 2))

(defcstruct hand-joints-motion-range-info-ext
  (type structure-type) ;; = type-hand-joints-motion-range-info-ext
  (next (:pointer (:pointer :void)))
  (hand-joints-motion-range hand-joints-motion-range-ext))

(defcenum hand-pose-type-msft
  (:tracked-msft 0)
  (:reference-open-palm-msft 1))

(defcstruct hand-mesh-space-create-info-msft
  (type structure-type) ;; = type-hand-mesh-space-create-info-msft
  (next (:pointer (:pointer :void)))
  (hand-pose-type hand-pose-type-msft)
  (pose-in-hand-mesh-space (:struct pose-f)))

(defcstruct hand-mesh-update-info-msft
  (type structure-type) ;; = type-hand-mesh-update-info-msft
  (next (:pointer (:pointer :void)))
  (time time)
  (hand-pose-type hand-pose-type-msft))

(defcstruct hand-mesh-vertex-msft
  (position (:struct vector-3f))
  (normal (:struct vector-3f)))

(defcstruct hand-mesh-vertex-buffer-msft
  (vertex-update-time time) ;; optional
  (vertex-capacity-input :uint32)
  (vertex-count-output :uint32) ;; optional
  (vertices (:pointer (:pointer (:struct hand-mesh-vertex-msft)))) ;; count vertex-capacity-input
)

(defcstruct hand-mesh-index-buffer-msft
  (index-buffer-key :uint32) ;; optional
  (index-capacity-input :uint32)
  (index-count-output :uint32) ;; optional
  (indices (:pointer (:pointer :uint32))) ;; count index-capacity-input
)

(defcstruct hand-mesh-msft
  (type structure-type) ;; = type-hand-mesh-msft
  (next (:pointer (:pointer :void)))
  (is-active bool-32)
  (index-buffer-changed bool-32)
  (vertex-buffer-changed bool-32)
  (index-buffer (:struct hand-mesh-index-buffer-msft))
  (vertex-buffer (:struct hand-mesh-vertex-buffer-msft)))

(defcstruct system-hand-tracking-mesh-properties-msft
  (type structure-type) ;; = type-system-hand-tracking-mesh-properties-msft
  (next (:pointer (:pointer :void)))
  (supports-hand-tracking-mesh bool-32)
  (max-hand-mesh-index-count :uint32)
  (max-hand-mesh-vertex-count :uint32))

(defcstruct hand-pose-type-info-msft
  (type structure-type) ;; = type-hand-pose-type-info-msft
  (next (:pointer (:pointer :void)))
  (hand-pose-type hand-pose-type-msft))

(defcstruct secondary-view-configuration-session-begin-info-msft
  (type structure-type) ;; = type-secondary-view-configuration-session-begin-info-msft
  (next (:pointer (:pointer :void)))
  (view-configuration-count :uint32)
  (enabled-view-configuration-types (:pointer
                                     (:pointer view-configuration-type))) ;; count view-configuration-count
)

(defcstruct secondary-view-configuration-state-msft
  (type structure-type) ;; = type-secondary-view-configuration-state-msft
  (next (:pointer (:pointer :void)))
  (view-configuration-type view-configuration-type)
  (active bool-32))

(defcstruct secondary-view-configuration-frame-state-msft
  (type structure-type) ;; = type-secondary-view-configuration-frame-state-msft
  (next (:pointer (:pointer :void)))
  (view-configuration-count :uint32)
  (view-configuration-states (:pointer
                              (:pointer
                               (:struct
                                secondary-view-configuration-state-msft)))) ;; count view-configuration-count
)

(defcstruct secondary-view-configuration-layer-info-msft
  (type structure-type) ;; = type-secondary-view-configuration-layer-info-msft
  (next (:pointer (:pointer :void)))
  (view-configuration-type view-configuration-type)
  (environment-blend-mode environment-blend-mode)
  (layer-count :uint32)
  (layers (:pointer
           (:pointer
            (:pointer (:pointer (:struct composition-layer-base-header)))))) ;; count layer-count
)

(defcstruct secondary-view-configuration-frame-end-info-msft
  (type structure-type) ;; = type-secondary-view-configuration-frame-end-info-msft
  (next (:pointer (:pointer :void)))
  (view-configuration-count :uint32)
  (view-configuration-layers-info (:pointer
                                   (:pointer
                                    (:struct
                                     secondary-view-configuration-layer-info-msft)))) ;; count view-configuration-count
)

(defcstruct secondary-view-configuration-swapchain-create-info-msft
  (type structure-type) ;; = type-secondary-view-configuration-swapchain-create-info-msft
  (next (:pointer (:pointer :void)))
  (view-configuration-type view-configuration-type))

(defcstruct holographic-window-attachment-msft
  (type structure-type) ;; = type-holographic-window-attachment-msft
  (next (:pointer (:pointer :void)))
  (holographic-space (:pointer (:pointer i-unknown)))
  (core-window (:pointer (:pointer i-unknown))))

(defcstruct android-surface-swapchain-create-info-fb
  (type structure-type) ;; = type-android-surface-swapchain-create-info-fb
  (next (:pointer (:pointer :void)))
  (create-flags android-surface-swapchain-flags-fb))

(defcstruct swapchain-state-base-header-fb
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct swapchain-state-android-surface-dimensions-fb
  (type structure-type) ;; = type-swapchain-state-android-surface-dimensions-fb
  (next (:pointer (:pointer :void)))
  (width :uint32)
  (height :uint32))

(defcstruct swapchain-state-sampler-opengl-esfb
  (type structure-type) ;; = type-swapchain-state-sampler-opengl-es-fb
  (next (:pointer (:pointer :void)))
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

(defcstruct swapchain-state-sampler-vulkan-fb
  (type structure-type) ;; = type-swapchain-state-sampler-vulkan-fb
  (next (:pointer (:pointer :void)))
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

(defcstruct composition-layer-secure-content-fb
  (type structure-type) ;; = type-composition-layer-secure-content-fb
  (next (:pointer (:pointer :void)))
  (flags composition-layer-secure-content-flags-fb))

(defcstruct loader-init-info-base-header-khr
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct loader-init-info-android-khr
  (type structure-type) ;; = type-loader-init-info-android-khr
  (next (:pointer (:pointer :void)))
  (application-vm (:pointer (:pointer :void)))
  (application-context (:pointer (:pointer :void))))

(defcstruct composition-layer-equirect-2-khr
  (type structure-type) ;; = type-composition-layer-equirect2-khr
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags) ;; optional
  (space space)
  (eye-visibility eye-visibility)
  (sub-image (:struct swapchain-sub-image))
  (pose (:struct pose-f))
  (radius :float)
  (central-horizontal-angle :float)
  (upper-vertical-angle :float)
  (lower-vertical-angle :float))

(defcstruct composition-layer-color-scale-bias-khr
  (type structure-type) ;; = type-composition-layer-color-scale-bias-khr
  (next (:pointer (:pointer :void)))
  (color-scale (:struct color-4f))
  (color-bias (:struct color-4f)))

(defcstruct controller-model-key-state-msft
  (type structure-type) ;; = type-controller-model-key-state-msft
  (next (:pointer (:pointer :void)))
  (model-key controller-model-key-msft))

(defcstruct controller-model-node-properties-msft
  (type structure-type) ;; = type-controller-model-node-properties-msft
  (next (:pointer (:pointer :void)))
  (parent-node-name :char :count +max-controller-model-node-name-size-msft+)
  (node-name :char :count +max-controller-model-node-name-size-msft+))

(defcstruct controller-model-properties-msft
  (type structure-type) ;; = type-controller-model-properties-msft
  (next (:pointer (:pointer :void)))
  (node-capacity-input :uint32) ;; optional
  (node-count-output :uint32) ;; optional
  (node-properties (:pointer
                    (:pointer (:struct controller-model-node-properties-msft)))) ;; count node-capacity-input, optional
)

(defcstruct controller-model-node-state-msft
  (type structure-type) ;; = type-controller-model-node-state-msft
  (next (:pointer (:pointer :void)))
  (node-pose (:struct pose-f)))

(defcstruct controller-model-state-msft
  (type structure-type) ;; = type-controller-model-state-msft
  (next (:pointer (:pointer :void)))
  (node-capacity-input :uint32) ;; optional
  (node-count-output :uint32) ;; optional
  (node-states (:pointer (:pointer (:struct controller-model-node-state-msft)))) ;; count node-capacity-input, optional
)

(defctype scene-observer-msft xr-handle)
(defctype scene-msft xr-handle)
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
  (:serialize-scene-msft 10000098000))

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
  (:serialized-scene-fragment-msft 10000098000))

(defcenum mesh-compute-lod-msft
  (:coarse-msft 1)
  (:edium-msft 2)
  (:fine-msft 3)
  (:unlimited-msft 4))

(defcstruct uuid-msft
  (bytes :uint8 :count 16))

(defcstruct scene-observer-create-info-msft
  (type structure-type) ;; = type-scene-observer-create-info-msft
  (next (:pointer (:pointer :void))))

(defcstruct scene-create-info-msft
  (type structure-type) ;; = type-scene-create-info-msft
  (next (:pointer (:pointer :void))))

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
  (spheres (:pointer (:pointer (:struct scene-sphere-bound-msft)))) ;; count sphere-count, optional
  (box-count :uint32) ;; optional
  (boxes (:pointer (:pointer (:struct scene-oriented-box-bound-msft)))) ;; count box-count, optional
  (frustum-count :uint32) ;; optional
  (frustums (:pointer (:pointer (:struct scene-frustum-bound-msft)))) ;; count frustum-count, optional
)

(defcstruct new-scene-compute-info-msft
  (type structure-type) ;; = type-new-scene-compute-info-msft
  (next (:pointer (:pointer :void)))
  (requested-feature-count :uint32)
  (requested-features (:pointer (:pointer scene-compute-feature-msft))) ;; count requested-feature-count
  (consistency scene-compute-consistency-msft)
  (bounds (:struct scene-bounds-msft)))

(defcstruct visual-mesh-compute-lod-info-msft
  (type structure-type) ;; = type-visual-mesh-compute-lod-info-msft
  (next (:pointer (:pointer :void)))
  (lod mesh-compute-lod-msft))

(defcstruct scene-component-msft
  (component-type scene-component-type-msft)
  (id (:struct uuid-msft))
  (parent-id (:struct uuid-msft)) ;; optional
  (update-time time))

(defcstruct scene-components-msft
  (type structure-type) ;; = type-scene-components-msft
  (next (:pointer (:pointer :void)))
  (component-capacity-input :uint32) ;; optional
  (component-count-output :uint32)
  (components (:pointer (:pointer (:struct scene-component-msft)))) ;; count component-capacity-input, optional
)

(defcstruct scene-components-get-info-msft
  (type structure-type) ;; = type-scene-components-get-info-msft
  (next (:pointer (:pointer :void)))
  (component-type scene-component-type-msft))

(defcstruct scene-component-location-msft
  (flags space-location-flags) ;; optional
  (pose (:struct pose-f)))

(defcstruct scene-component-locations-msft
  (type structure-type) ;; = type-scene-component-locations-msft
  (next (:pointer (:pointer :void)))
  (location-count :uint32) ;; optional
  (locations (:pointer (:pointer (:struct scene-component-location-msft)))) ;; count location-count, optional
)

(defcstruct scene-components-locate-info-msft
  (type structure-type) ;; = type-scene-components-locate-info-msft
  (next (:pointer (:pointer :void)))
  (base-space space)
  (time time)
  (component-id-count :uint32) ;; optional
  (component-ids (:pointer (:pointer (:struct uuid-msft)))) ;; count component-id-count, optional
)

(defcstruct scene-object-msft
  (object-type scene-object-type-msft))

(defcstruct scene-objects-msft
  (type structure-type) ;; = type-scene-objects-msft
  (next (:pointer (:pointer :void)))
  (scene-object-count :uint32) ;; optional
  (scene-objects (:pointer (:pointer (:struct scene-object-msft)))) ;; count scene-object-count, optional
)

(defcstruct scene-component-parent-filter-info-msft
  (type structure-type) ;; = type-scene-component-parent-filter-info-msft
  (next (:pointer (:pointer :void)))
  (parent-id (:struct uuid-msft)))

(defcstruct scene-object-types-filter-info-msft
  (type structure-type) ;; = type-scene-object-types-filter-info-msft
  (next (:pointer (:pointer :void)))
  (object-type-count :uint32) ;; optional
  (object-types (:pointer (:pointer scene-object-type-msft))) ;; count object-type-count, optional
)

(defcstruct scene-plane-msft
  (alignment scene-plane-alignment-type-msft)
  (size (:struct extent-2d-f))
  (mesh-buffer-id :uint64)
  (supports-indices-uint-16 bool-32))

(defcstruct scene-planes-msft
  (type structure-type) ;; = type-scene-planes-msft
  (next (:pointer (:pointer :void)))
  (scene-plane-count :uint32) ;; optional
  (scene-planes (:pointer (:pointer (:struct scene-plane-msft)))) ;; count scene-plane-count, optional
)

(defcstruct scene-plane-alignment-filter-info-msft
  (type structure-type) ;; = type-scene-plane-alignment-filter-info-msft
  (next (:pointer (:pointer :void)))
  (alignment-count :uint32) ;; optional
  (alignments (:pointer (:pointer scene-plane-alignment-type-msft))) ;; count alignment-count, optional
)

(defcstruct scene-mesh-msft
  (mesh-buffer-id :uint64)
  (supports-indices-uint-16 bool-32))

(defcstruct scene-meshes-msft
  (type structure-type) ;; = type-scene-meshes-msft
  (next (:pointer (:pointer :void)))
  (scene-mesh-count :uint32) ;; optional
  (scene-meshes (:pointer (:pointer (:struct scene-mesh-msft)))) ;; count scene-mesh-count, optional
)

(defcstruct scene-mesh-buffers-get-info-msft
  (type structure-type) ;; = type-scene-mesh-buffers-get-info-msft
  (next (:pointer (:pointer :void)))
  (mesh-buffer-id :uint64))

(defcstruct scene-mesh-buffers-msft
  (type structure-type) ;; = type-scene-mesh-buffers-msft
  (next (:pointer (:pointer :void))))

(defcstruct scene-mesh-vertex-buffer-msft
  (type structure-type) ;; = type-scene-mesh-vertex-buffer-msft
  (next (:pointer (:pointer :void)))
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32)
  (vertices (:pointer (:pointer (:struct vector-3f)))) ;; count vertex-capacity-input, optional
)

(defcstruct scene-mesh-indices-uint-32-msft
  (type structure-type) ;; = type-scene-mesh-indices-uint32-msft
  (next (:pointer (:pointer :void)))
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32)
  (indices (:pointer (:pointer :uint32))) ;; count index-capacity-input, optional
)

(defcstruct scene-mesh-indices-uint-16-msft
  (type structure-type) ;; = type-scene-mesh-indices-uint16-msft
  (next (:pointer (:pointer :void)))
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32)
  (indices (:pointer (:pointer :uint16))) ;; count index-capacity-input, optional
)

(defcstruct serialized-scene-fragment-data-get-info-msft
  (type structure-type) ;; = type-serialized-scene-fragment-data-get-info-msft
  (next (:pointer (:pointer :void)))
  (scene-fragment-id (:struct uuid-msft)))

(defcstruct deserialize-scene-fragment-msft
  (buffer-size :uint32) ;; optional
  (buffer (:pointer (:pointer :uint8))) ;; count buffer-size, optional
)

(defcstruct scene-deserialize-info-msft
  (type structure-type) ;; = type-scene-deserialize-info-msft
  (next (:pointer (:pointer :void)))
  (fragment-count :uint32) ;; optional
  (fragments (:pointer (:pointer (:struct deserialize-scene-fragment-msft)))) ;; count fragment-count, optional
)

(defcenum color-space-fb
  (:unmanaged-fb 0)
  (:rec2020-fb 1)
  (:rec709-fb 2)
  (:rift-cv1-fb 3)
  (:rift-s-fb 4)
  (:quest-fb 5)
  (:p3-fb 6)
  (:adobe-rgb-fb 7))

(defcstruct system-color-space-properties-fb
  (type structure-type) ;; = type-system-color-space-properties-fb
  (next (:pointer (:pointer :void)))
  (color-space color-space-fb))

(defcstruct system-spatial-entity-properties-fb
  (type structure-type) ;; = type-system-spatial-entity-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-spatial-entity bool-32))

(defcstruct spatial-anchor-create-info-fb
  (type structure-type) ;; = type-spatial-anchor-create-info-fb
  (next (:pointer (:pointer :void)))
  (space space)
  (pose-in-space (:struct pose-f))
  (time time))

(defcstruct space-component-status-set-info-fb
  (type structure-type) ;; = type-space-component-status-set-info-fb
  (next (:pointer (:pointer :void)))
  (component-type space-component-type-fb)
  (enabled bool-32)
  (timeout duration))

(defcstruct space-component-status-fb
  (type structure-type) ;; = type-space-component-status-fb
  (next (:pointer (:pointer :void)))
  (enabled bool-32)
  (change-pending bool-32))

(defcstruct uuid-ext
  (data :uint8 :count +uuid-size-ext+))

(defcstruct event-data-spatial-anchor-create-complete-fb
  (type structure-type) ;; = type-event-data-spatial-anchor-create-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext)))

(defcstruct event-data-space-set-status-complete-fb
  (type structure-type) ;; = type-event-data-space-set-status-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext))
  (component-type space-component-type-fb)
  (enabled bool-32))

(defcstruct foveation-profile-create-info-fb
  (type structure-type) ;; = type-foveation-profile-create-info-fb
  (next (:pointer (:pointer :void))))

(defcstruct swapchain-create-info-foveation-fb
  (type structure-type) ;; = type-swapchain-create-info-foveation-fb
  (next (:pointer (:pointer :void)))
  (flags swapchain-create-foveation-flags-fb) ;; optional
)

(defcstruct swapchain-state-foveation-fb
  (type structure-type) ;; = type-swapchain-state-foveation-fb
  (next (:pointer (:pointer :void)))
  (flags swapchain-state-foveation-flags-fb) ;; optional
  (profile foveation-profile-fb))

(defcstruct swapchain-image-foveation-vulkan-fb
  (type structure-type) ;; = type-swapchain-image-foveation-vulkan-fb
  (next (:pointer (:pointer :void)))
  (image vk-image)
  (width :uint32)
  (height :uint32))

(defcenum foveation-level-fb
  ;; no foveation
  (:none-fb 0)
  ;; less foveation (higher periphery visual fidelity, lower performance)
  (:low-fb 1)
  ;; medium foveation (medium periphery visual fidelity, medium performance)
  (:medium-fb 2)
  ;; high foveation (lower periphery visual fidelity, higher performance)
  (:high-fb 3))

(defcenum foveation-dynamic-fb
  ;; static foveation at the maximum desired level
  (:disabled-fb 0)
  ;; dynamic changing foveation based on performance headroom available up to the maximum desired level
  (:level-enabled-fb 1))

(defcstruct foveation-level-profile-create-info-fb
  (type structure-type) ;; = type-foveation-level-profile-create-info-fb
  (next (:pointer (:pointer :void)))
  (level foveation-level-fb)
  (vertical-offset :float)
  (dynamic foveation-dynamic-fb))

(defcstruct foveation-eye-tracked-profile-create-info-meta
  (type structure-type) ;; = type-foveation-eye-tracked-profile-create-info-meta
  (next (:pointer (:pointer :void)))
  (flags foveation-eye-tracked-profile-create-flags-meta))

(defcstruct foveation-eye-tracked-state-meta
  (type structure-type) ;; = type-foveation-eye-tracked-state-meta
  (next (:pointer (:pointer :void)))
  (foveation-center (:struct vector-2f) :count +foveation-center-size-meta+)
  (flags foveation-eye-tracked-state-flags-meta))

(defcstruct system-foveation-eye-tracked-properties-meta
  (type structure-type) ;; = type-system-foveation-eye-tracked-properties-meta
  (next (:pointer (:pointer :void)))
  (supports-foveation-eye-tracked bool-32))

(defcstruct vector-4s-fb
  (x :int16)
  (y :int16)
  (z :int16)
  (w :int16))

(defcstruct hand-tracking-mesh-fb
  (type structure-type) ;; = type-hand-tracking-mesh-fb
  (next (:pointer (:pointer :void)))
  (joint-capacity-input :uint32) ;; optional
  (joint-count-output :uint32) ;; optional
  (joint-bind-poses (:pointer (:pointer (:struct pose-f)))) ;; count joint-capacity-input, optional
  (joint-radi-i (:pointer (:pointer :float))) ;; count joint-capacity-input, optional
  (joint-parents (:pointer (:pointer hand-joint-ext))) ;; count joint-capacity-input, optional
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32) ;; optional
  (vertex-positions (:pointer (:pointer (:struct vector-3f)))) ;; count vertex-capacity-input, optional
  (vertex-normals (:pointer (:pointer (:struct vector-3f)))) ;; count vertex-capacity-input, optional
  (vertex-uvs (:pointer (:pointer (:struct vector-2f)))) ;; count vertex-capacity-input, optional
  (vertex-blend-indices (:pointer (:pointer (:struct vector-4s-fb)))) ;; count vertex-capacity-input, optional
  (vertex-blend-weights (:pointer (:pointer (:struct vector-4f)))) ;; count vertex-capacity-input, optional
  (index-capacity-input :uint32) ;; optional
  (index-count-output :uint32) ;; optional
  (indices (:pointer (:pointer :int16))) ;; count index-capacity-input, optional
)

(defcstruct hand-tracking-scale-fb
  (type structure-type) ;; = type-hand-tracking-scale-fb
  (next (:pointer (:pointer :void)))
  (sensor-output :float)
  (current-output :float)
  (override-hand-scale bool-32)
  (override-value-input :float) ;; optional
)

(defcstruct hand-tracking-aim-state-fb
  (type structure-type) ;; = type-hand-tracking-aim-state-fb
  (next (:pointer (:pointer :void)))
  (status hand-tracking-aim-flags-fb)
  (aim-pose (:struct pose-f))
  (pinch-strength-index :float)
  (pinch-strength-middle :float)
  (pinch-strength-ring :float)
  (pinch-strength-little :float))

(defcstruct hand-capsule-fb
  (points (:struct vector-3f) :count +hand-tracking-capsule-point-count-fb+)
  (radius :float)
  (joint hand-joint-ext))

(defcstruct hand-tracking-capsules-state-fb
  (type structure-type) ;; = type-hand-tracking-capsules-state-fb
  (next (:pointer (:pointer :void)))
  (capsules (:struct hand-capsule-fb) :count +hand-tracking-capsule-count-fb+))

(defcstruct render-model-path-info-fb
  (type structure-type) ;; = type-render-model-path-info-fb
  (next (:pointer (:pointer :void)))
  (path path))

(defcstruct render-model-properties-fb
  (type structure-type) ;; = type-render-model-properties-fb
  (next (:pointer (:pointer :void)))
  (vendor-id :uint32)
  (model-name :char :count +max-render-model-name-size-fb+)
  (model-key render-model-key-fb)
  (model-version :uint32)
  (flags render-model-flags-fb))

(defcstruct render-model-buffer-fb
  (type structure-type) ;; = type-render-model-buffer-fb
  (next (:pointer (:pointer :void)))
  (buffer-capacity-input :uint32) ;; optional
  (buffer-count-output :uint32) ;; optional
  (buffer (:pointer (:pointer :uint8))) ;; count buffer-capacity-input, optional
)

(defcstruct render-model-load-info-fb
  (type structure-type) ;; = type-render-model-load-info-fb
  (next (:pointer (:pointer :void)))
  (model-key render-model-key-fb))

(defcstruct system-render-model-properties-fb
  (type structure-type) ;; = type-system-render-model-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-render-model-loading bool-32))

(defcstruct render-model-capabilities-request-fb
  (type structure-type) ;; = type-render-model-capabilities-request-fb
  (next (:pointer (:pointer :void)))
  (flags render-model-flags-fb))

(defcstruct space-query-info-base-header-fb
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct space-filter-info-base-header-fb
  (type structure-type)
  (next (:pointer (:pointer :void))))

(defcstruct space-query-info-fb
  (type structure-type) ;; = type-space-query-info-fb
  (next (:pointer (:pointer :void)))
  (query-action space-query-action-fb)
  (max-result-count :uint32)
  (timeout duration)
  (filter (:pointer (:pointer (:struct space-filter-info-base-header-fb)))) ;; optional
  (exclude-filter (:pointer (:pointer (:struct space-filter-info-base-header-fb)))) ;; optional
)

(defcstruct space-storage-location-filter-info-fb
  (type structure-type) ;; = type-space-storage-location-filter-info-fb
  (next (:pointer (:pointer :void)))
  (location space-storage-location-fb))

(defcstruct space-uuid-filter-info-fb
  (type structure-type) ;; = type-space-uuid-filter-info-fb
  (next (:pointer (:pointer :void)))
  (uuid-count :uint32)
  (uuids (:pointer (:pointer (:struct uuid-ext)))) ;; count uuid-count
)

(defcstruct space-component-filter-info-fb
  (type structure-type) ;; = type-space-component-filter-info-fb
  (next (:pointer (:pointer :void)))
  (component-type space-component-type-fb))

(defcstruct space-query-result-fb
  (space space)
  (uuid (:struct uuid-ext)))

(defcstruct space-query-results-fb
  (type structure-type) ;; = type-space-query-results-fb
  (next (:pointer (:pointer :void)))
  (result-capacity-input :uint32) ;; optional
  (result-count-output :uint32) ;; optional
  (results (:pointer (:pointer (:struct space-query-result-fb)))) ;; count result-capacity-input, optional
)

(defcstruct event-data-space-query-results-available-fb
  (type structure-type) ;; = type-event-data-space-query-results-available-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb))

(defcstruct event-data-space-query-complete-fb
  (type structure-type) ;; = type-event-data-space-query-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result))

(defcstruct space-save-info-fb
  (type structure-type) ;; = type-space-save-info-fb
  (next (:pointer (:pointer :void)))
  (space space)
  (location space-storage-location-fb)
  (persistence-mode space-persistence-mode-fb))

(defcstruct space-erase-info-fb
  (type structure-type) ;; = type-space-erase-info-fb
  (next (:pointer (:pointer :void)))
  (space space)
  (location space-storage-location-fb))

(defcstruct event-data-space-save-complete-fb
  (type structure-type) ;; = type-event-data-space-save-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext))
  (location space-storage-location-fb))

(defcstruct event-data-space-erase-complete-fb
  (type structure-type) ;; = type-event-data-space-erase-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result)
  (space space)
  (uuid (:struct uuid-ext))
  (location space-storage-location-fb))

(defcstruct space-share-info-fb
  (type structure-type) ;; = type-space-share-info-fb
  (next (:pointer (:pointer :void)))
  (space-count :uint32)
  (spaces (:pointer (:pointer space))) ;; count space-count
  (user-count :uint32)
  (users (:pointer (:pointer space-user-fb))) ;; count user-count
)

(defcstruct event-data-space-share-complete-fb
  (type structure-type) ;; = type-event-data-space-share-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result))

(defcstruct space-list-save-info-fb
  (type structure-type) ;; = type-space-list-save-info-fb
  (next (:pointer (:pointer :void)))
  (space-count :uint32)
  (spaces (:pointer (:pointer space))) ;; count space-count
  (location space-storage-location-fb))

(defcstruct event-data-space-list-save-complete-fb
  (type structure-type) ;; = type-event-data-space-list-save-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result))

(defcstruct space-container-fb
  (type structure-type) ;; = type-space-container-fb
  (next (:pointer (:pointer :void)))
  (uuid-capacity-input :uint32) ;; optional
  (uuid-count-output :uint32) ;; optional
  (uuids (:pointer (:pointer (:struct uuid-ext)))) ;; count uuid-capacity-input, optional
)

(defcstruct extent-3df-fb
  (width :float)
  (height :float)
  (depth :float))

(defcstruct offset-3df-fb
  (x :float)
  (y :float)
  (z :float))

(defcstruct rect-3df-fb
  (offset (:struct offset-3df-fb))
  (extent (:struct extent-3df-fb)))

(defcstruct semantic-labels-fb
  (type structure-type) ;; = type-semantic-labels-fb
  (next (:pointer (:pointer :void)))
  (buffer-capacity-input :uint32) ;; optional
  (buffer-count-output :uint32) ;; optional
  (buffer (:pointer (:pointer :char))) ;; count buffer-capacity-input, optional
)

(defcstruct room-layout-fb
  (type structure-type) ;; = type-room-layout-fb
  (next (:pointer (:pointer :void)))
  (floor-uuid (:struct uuid-ext))
  (ceiling-uuid (:struct uuid-ext))
  (wall-uuid-capacity-input :uint32) ;; optional
  (wall-uuid-count-output :uint32) ;; optional
  (wall-uuids (:pointer (:pointer (:struct uuid-ext)))) ;; count wall-uuid-capacity-input, optional
)

(defcstruct boundary-2dfb
  (type structure-type) ;; = type-boundary-2d-fb
  (next (:pointer (:pointer :void)))
  (vertex-capacity-input :uint32) ;; optional
  (vertex-count-output :uint32) ;; optional
  (vertices (:pointer (:pointer (:struct vector-2f)))) ;; count vertex-capacity-input, optional
)

(defcstruct scene-capture-request-info-fb
  (type structure-type) ;; = type-scene-capture-request-info-fb
  (next (:pointer (:pointer :void)))
  (request-byte-count :uint32) ;; optional
  (request (:pointer (:pointer :char))) ;; count request-byte-count, optional
)

(defcstruct event-data-scene-capture-complete-fb
  (type structure-type) ;; = type-event-data-scene-capture-complete-fb
  (next (:pointer (:pointer :void)))
  (request-id async-request-id-fb)
  (result result))

(defcstruct system-keyboard-tracking-properties-fb
  (type structure-type) ;; = type-system-keyboard-tracking-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-keyboard-tracking bool-32))

(defcstruct keyboard-tracking-description-fb
  (tracked-keyboard-id :uint64)
  (size (:struct vector-3f))
  (flags keyboard-tracking-flags-fb)
  (name :char :count +max-keyboard-tracking-name-size-fb+))

(defcstruct keyboard-space-create-info-fb
  (type structure-type) ;; = type-keyboard-space-create-info-fb
  (next (:pointer (:pointer :void)))
  (tracked-keyboard-id :uint64))

(defcstruct keyboard-tracking-query-fb
  (type structure-type) ;; = type-keyboard-tracking-query-fb
  (next (:pointer (:pointer :void)))
  (flags keyboard-tracking-query-flags-fb))

(defcstruct composition-layer-depth-test-varjo
  (type structure-type) ;; = type-composition-layer-depth-test-varjo
  (next (:pointer (:pointer :void)))
  (depth-test-range-near-z :float)
  (depth-test-range-far-z :float))

(defcstruct view-locate-foveated-rendering-varjo
  (type structure-type) ;; = type-view-locate-foveated-rendering-varjo
  (next (:pointer (:pointer :void)))
  (foveated-rendering-active bool-32))

(defcstruct foveated-view-configuration-view-varjo
  (type structure-type) ;; = type-foveated-view-configuration-view-varjo
  (next (:pointer (:pointer :void)))
  (foveated-rendering-active bool-32))

(defcstruct system-foveated-rendering-properties-varjo
  (type structure-type) ;; = type-system-foveated-rendering-properties-varjo
  (next (:pointer (:pointer :void)))
  (supports-foveated-rendering bool-32))

(defcenum reprojection-mode-msft
  (:depth-msft 1)
  (:planar-from-depth-msft 2)
  (:planar-manual-msft 3)
  (:orientation-only-msft 4))

(defcstruct composition-layer-reprojection-info-msft
  (type structure-type) ;; = type-composition-layer-reprojection-info-msft
  (next (:pointer (:pointer :void)))
  (reprojection-mode reprojection-mode-msft))

(defcstruct composition-layer-reprojection-plane-override-msft
  (type structure-type) ;; = type-composition-layer-reprojection-plane-override-msft
  (next (:pointer (:pointer :void)))
  (position (:struct vector-3f))
  (normal (:struct vector-3f))
  (velocity (:struct vector-3f)))

(defcstruct triangle-mesh-create-info-fb
  (type structure-type) ;; = type-triangle-mesh-create-info-fb
  (next (:pointer (:pointer :void)))
  (flags triangle-mesh-flags-fb) ;; optional
  (winding-order winding-order-fb)
  (vertex-count :uint32)
  (vertex-buffer (:pointer (:pointer (:struct vector-3f)))) ;; noautovalidity optional
  (triangle-count :uint32)
  (index-buffer (:pointer (:pointer :uint32))) ;; noautovalidity optional
)

(defcstruct system-passthrough-properties-fb
  (type structure-type) ;; = type-system-passthrough-properties-fb
  (next (:pointer (:pointer :void)))
  (supports-passthrough bool-32))

(defcstruct system-passthrough-properties-2fb
  (type structure-type) ;; = type-system-passthrough-properties2-fb
  (next (:pointer (:pointer :void)))
  (capabilities passthrough-capability-flags-fb))

(defcstruct passthrough-create-info-fb
  (type structure-type) ;; = type-passthrough-create-info-fb
  (next (:pointer (:pointer :void)))
  (flags passthrough-flags-fb))

(defcstruct passthrough-layer-create-info-fb
  (type structure-type) ;; = type-passthrough-layer-create-info-fb
  (next (:pointer (:pointer :void)))
  (passthrough passthrough-fb)
  (flags passthrough-flags-fb)
  (purpose passthrough-layer-purpose-fb))

(defcstruct composition-layer-passthrough-fb
  (type structure-type) ;; = type-composition-layer-passthrough-fb
  (next (:pointer (:pointer :void)))
  (flags composition-layer-flags)
  (space space)
  (layer-handle passthrough-layer-fb))

(defcstruct geometry-instance-create-info-fb
  (type structure-type) ;; = type-geometry-instance-create-info-fb
  (next (:pointer (:pointer :void)))
  (layer passthrough-layer-fb)
  (mesh triangle-mesh-fb)
  (base-space space)
  (pose (:struct pose-f))
  (scale (:struct vector-3f)))

(defcstruct geometry-instance-transform-fb
  (type structure-type) ;; = type-geometry-instance-transform-fb
  (next (:pointer (:pointer :void)))
  (base-space space)
  (time time)
  (pose (:struct pose-f))
  (scale (:struct vector-3f)))

(defcstruct passthrough-style-fb
  (type structure-type) ;; = type-passthrough-style-fb
  (next (:pointer (:pointer :void)))
  (texture-opacity-factor :float)
  (edge-color (:struct color-4f)))

(defcstruct passthrough-color-map-mono-to-rgba-fb
  (type structure-type) ;; = type-passthrough-color-map-mono-to-rgba-fb
  (next (:pointer (:pointer :void)))
  (texture-color-map (:struct color-4f) :count +passthrough-color-map-mono-size-fb+))

(defcstruct passthrough-color-map-mono-to-mono-fb
  (type structure-type) ;; = type-passthrough-color-map-mono-to-mono-fb
  (next (:pointer (:pointer :void)))
  (texture-color-map :uint8 :count +passthrough-color-map-mono-size-fb+))

(defcstruct passthrough-brightness-contrast-saturation-fb
  (type structure-type) ;; = type-passthrough-brightness-contrast-saturation-fb
  (next (:pointer (:pointer :void)))
  (brightness :float)
  (contrast :float)
  (saturation :float))

(defcstruct event-data-passthrough-state-changed-fb
  (type structure-type) ;; = type-event-data-passthrough-state-changed-fb
  (next (:pointer (:pointer :void)))
  (flags passthrough-state-changed-flags-fb))

(defcstruct passthrough-keyboard-hands-intensity-fb
  (type structure-type) ;; = type-passthrough-keyboard-hands-intensity-fb
  (next (:pointer (:pointer :void)))
  (left-hand-intensity :float)
  (right-hand-intensity :float))

(defcstruct local-dimming-frame-end-info-meta
  (type structure-type) ;; = type-local-dimming-frame-end-info-meta
  (next (:pointer (:pointer :void)))
  (local-dimming-mode local-dimming-mode-meta))

(defctype spatial-anchor-store-connection-msft xr-handle)
(defcstruct spatial-anchor-persistence-name-msft
  (name :char :count +max-spatial-anchor-name-size-msft+))

(defcstruct spatial-anchor-persistence-info-msft
  (type structure-type) ;; = type-spatial-anchor-persistence-info-msft
  (next (:pointer (:pointer :void)))
  (spatial-anchor-persistence-name (:struct spatial-anchor-persistence-name-msft))
  (spatial-anchor spatial-anchor-msft))

(defcstruct spatial-anchor-from-persisted-anchor-create-info-msft
  (type structure-type) ;; = type-spatial-anchor-from-persisted-anchor-create-info-msft
  (next (:pointer (:pointer :void)))
  (spatial-anchor-store spatial-anchor-store-connection-msft)
  (spatial-anchor-persistence-name (:struct spatial-anchor-persistence-name-msft)))

(defcstruct facial-tracker-create-info-htc
  (type structure-type) ;; = type-facial-tracker-create-info-htc
  (next (:pointer (:pointer :void)))
  (facial-tracking-type facial-tracking-type-htc))

(defcstruct system-facial-tracking-properties-htc
  (type structure-type) ;; = type-system-facial-tracking-properties-htc
  (next (:pointer (:pointer :void)))
  (support-eye-facial-tracking bool-32)
  (support-lip-facial-tracking bool-32))

(defcstruct facial-expressions-htc
  (type structure-type) ;; = type-facial-expressions-htc
  (next (:pointer (:pointer :void)))
  (is-active bool-32)
  (sample-time time)
  (expression-count :uint32)
  (expression-weightings (:pointer (:pointer :float))))

(defcstruct passthrough-create-info-htc
  (type structure-type) ;; = type-passthrough-create-info-htc
  (next (:pointer (:pointer :void)))
  (form passthrough-form-htc))

(defcstruct passthrough-color-htc
  (type structure-type) ;; = type-passthrough-color-htc
  (next (:pointer (:pointer :void)))
  (alpha :float))

(defcstruct passthrough-mesh-transform-info-htc
  (type structure-type) ;; = type-passthrough-mesh-transform-info-htc
  (next (:pointer (:pointer :void)))
  (vertex-count :uint32)
  (vertices (:pointer (:pointer (:struct vector-3f)))) ;; count vertex-count
  (index-count :uint32)
  (indices (:pointer (:pointer :uint32))) ;; count index-count
  (base-space space)
  (time time)
  (pose (:struct pose-f))
  (scale (:struct vector-3f)))

(defcstruct composition-layer-passthrough-htc
  (type structure-type) ;; = type-composition-layer-passthrough-htc
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-flags)
  (space space)
  (passthrough passthrough-htc)
  (color (:struct passthrough-color-htc)))

(defcstruct vive-tracker-paths-htcx
  (type structure-type) ;; = type-vive-tracker-paths-htcx
  (next (:pointer (:pointer :void)))
  (persistent-path path)
  (role-path path) ;; optional
)

(defcstruct event-data-vive-tracker-connected-htcx
  (type structure-type) ;; = type-event-data-vive-tracker-connected-htcx
  (next (:pointer (:pointer :void)))
  (paths (:pointer (:pointer (:struct vive-tracker-paths-htcx)))))

(defcstruct composition-layer-space-warp-info-fb
  (type structure-type) ;; = type-composition-layer-space-warp-info-fb
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-space-warp-info-flags-fb) ;; optional
  (motion-vector-sub-image (:struct swapchain-sub-image))
  (app-space-delta-pose (:struct pose-f))
  (depth-sub-image (:struct swapchain-sub-image))
  (min-depth :float)
  (max-depth :float)
  (near-z :float)
  (far-z :float))

(defcstruct system-space-warp-properties-fb
  (type structure-type) ;; = type-system-space-warp-properties-fb
  (next (:pointer (:pointer :void)))
  (recommended-motion-vector-image-rect-width :uint32)
  (recommended-motion-vector-image-rect-height :uint32))

(defcstruct system-marker-tracking-properties-varjo
  (type structure-type) ;; = type-system-marker-tracking-properties-varjo
  (next (:pointer (:pointer :void)))
  (supports-marker-tracking bool-32))

(defcstruct event-data-marker-tracking-update-varjo
  (type structure-type) ;; = type-event-data-marker-tracking-update-varjo
  (next (:pointer (:pointer :void)))
  (marker-id :uint64)
  (is-active bool-32)
  (is-predicted bool-32)
  (time time))

(defcstruct marker-space-create-info-varjo
  (type structure-type) ;; = type-marker-space-create-info-varjo
  (next (:pointer (:pointer :void)))
  (marker-id :uint64)
  (pose-in-marker-space (:struct pose-f)))

(defcstruct global-dimmer-frame-end-info-ml
  (type structure-type) ;; = type-global-dimmer-frame-end-info-ml
  (next (:pointer (:pointer :void)))
  (dimmer-value :float)
  (flags global-dimmer-frame-end-info-flags-ml) ;; optional
)

(defcstruct digital-lens-control-almalence
  (type structure-type) ;; = type-digital-lens-control-almalence
  (next (:pointer (:pointer :void)))
  (flags digital-lens-control-flags-almalence))

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

(defcenum compare-op-fb
  ;; comparison is never true.
  (:never-fb 0)
  ;; comparison is true if source less than is destination.
  (:less-fb 1)
  ;; comparison is true if source is equal to destination.
  (:equal-fb 2)
  ;; comparison is true if source is less than or equal to destination.
  (:less-or-equal-fb 3)
  ;; comparison is true if source is greater than destination.
  (:greater-fb 4)
  ;; comparison is true if source is not equal to destination.
  (:not-equal-fb 5)
  ;; comparison is true if source is greater than or equal to destination.
  (:greater-or-equal-fb 6)
  ;; comparison is always true.
  (:always-fb 7))

(defcstruct composition-layer-settings-fb
  (type structure-type) ;; = type-composition-layer-settings-fb
  (next (:pointer (:pointer :void)))
  (layer-flags composition-layer-settings-flags-fb))

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

(defcstruct external-camera-oculus
  (type structure-type) ;; = type-external-camera-oculus
  (next (:pointer (:pointer :void)))
  (name :char :count +max-external-camera-name-size-oculus+)
  (intrinsics (:struct external-camera-intrinsics-oculus))
  (extrinsics (:struct external-camera-extrinsics-oculus)))

(defcstruct performance-metrics-state-meta
  (type structure-type) ;; = type-performance-metrics-state-meta
  (next (:pointer (:pointer :void)))
  (enabled bool-32))

(defcstruct performance-metrics-counter-meta
  (type structure-type) ;; = type-performance-metrics-counter-meta
  (next (:pointer (:pointer :void)))
  (counter-flags performance-metrics-counter-flags-meta) ;; optional
  (counter-unit performance-metrics-counter-unit-meta)
  (uint-value :uint32)
  (float-value :float))

(defcstruct system-headset-id-properties-meta
  (type structure-type) ;; = type-system-headset-id-properties-meta
  (next (:pointer (:pointer :void)))
  (id (:struct uuid-ext)))

(defcstruct foveation-apply-info-htc
  (type structure-type) ;; = type-foveation-apply-info-htc
  (next (:pointer (:pointer :void)))
  (mode foveation-mode-htc)
  (sub-image-count :uint32)
  (sub-images (:pointer (:pointer (:struct swapchain-sub-image)))) ;; count sub-image-count
)

(defcstruct foveation-configuration-htc
  (level foveation-level-htc)
  (clear-fov-degree :float)
  (focal-center-offset (:struct vector-2f)))

(defcstruct foveation-dynamic-mode-info-htc
  (type structure-type) ;; = type-foveation-dynamic-mode-info-htc
  (next (:pointer (:pointer :void)))
  (dynamic-flags foveation-dynamic-flags-htc) ;; optional
)

(defcstruct foveation-custom-mode-info-htc
  (type structure-type) ;; = type-foveation-custom-mode-info-htc
  (next (:pointer (:pointer :void)))
  (config-count :uint32)
  (configs (:pointer (:pointer (:struct foveation-configuration-htc)))) ;; count config-count
)

(defcstruct active-action-set-priority-ext
  (action-set action-set)
  (priority-override :uint32))

(defcstruct active-action-set-priorities-ext
  (type structure-type) ;; = type-active-action-set-priorities-ext
  (next (:pointer (:pointer :void)))
  (action-set-priority-count :uint32)
  (action-set-priorities (:pointer
                          (:pointer (:struct active-action-set-priority-ext)))) ;; count action-set-priority-count
)

(defcstruct composition-layer-depth-test-fb
  (type structure-type) ;; = type-composition-layer-depth-test-fb
  (next (:pointer (:pointer :void)))
  (depth-mask bool-32)
  (compare-op compare-op-fb))

(defcstruct coordinate-space-create-info-ml
  (type structure-type) ;; = type-coordinate-space-create-info-ml
  (next (:pointer (:pointer :void)))
  (cfuid ml-coordinate-frame-uid)
  (pose-in-coordinate-space (:struct pose-f)))

(defcstruct frame-end-info-ml
  (type structure-type) ;; = type-frame-end-info-ml
  (next (:pointer (:pointer :void)))
  (focus-distance :float)
  (flags frame-end-info-flags-ml) ;; optional
)

(defcstruct haptic-amplitude-envelope-vibration-fb
  (type structure-type) ;; = type-haptic-amplitude-envelope-vibration-fb
  (next (:pointer (:pointer :void)))
  (duration duration)
  (amplitude-count :uint32)
  (amplitudes (:pointer (:pointer :float))) ;; count amplitude-count
)

(defcstruct haptic-pcm-vibration-fb
  (type structure-type) ;; = type-haptic-pcm-vibration-fb
  (next (:pointer (:pointer :void)))
  (buffer-size :uint32)
  (buffer (:pointer (:pointer :float))) ;; count buffer-size
  (sample-rate :float)
  (append bool-32)
  (samples-consumed (:pointer (:pointer :uint32))))

(defcstruct device-pcm-sample-rate-state-fb
  (type structure-type) ;; = type-device-pcm-sample-rate-state-fb
  (next (:pointer (:pointer :void)))
  (sample-rate :float))

(defcstruct device-pcm-sample-rate-get-info-fb)

(defcstruct space-user-create-info-fb
  (type structure-type) ;; = type-space-user-create-info-fb
  (next (:pointer (:pointer :void)))
  (user-id space-user-id-fb))

(defcstruct system-force-feedback-curl-properties-mndx
  (type structure-type) ;; = type-system-force-feedback-curl-properties-mndx
  (next (:pointer (:pointer :void)))
  (supports-force-feedback-curl bool-32))

(defcstruct force-feedback-curl-apply-location-mndx
  (location force-feedback-curl-location-mndx)
  (value :float))

(defcstruct force-feedback-curl-apply-locations-mndx
  (type structure-type) ;; = type-force-feedback-curl-apply-locations-mndx
  (next (:pointer (:pointer :void)))
  (location-count :uint32)
  (locations (:pointer
              (:pointer (:struct force-feedback-curl-apply-location-mndx)))) ;; count location-count
)

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-out-of-memory)
(defcfun ("xrGetInstanceProcAddr" get-instance-proc-addr) result
  ;; optional = true
 (instance instance)
  ;; count = null-terminated
 (name (:pointer :string))
 (function (:pointer :pointer)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-out-of-memory
;;          error-size-insufficient)
(defcfun ("xrEnumerateApiLayerProperties" enumerate-api-layer-properties) result
  ;; optional = true
 (property-capacity-input :uint32)
 (property-count-output (:pointer :uint32))
  ;; count = property-capacity-input
  ;; optional = true
 (properties (:pointer (:struct api-layer-properties))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-out-of-memory
;;          error-size-insufficient error-runtime-unavailable
;;          error-api-layer-not-present)
(defcfun ("xrEnumerateInstanceExtensionProperties" enumerate-instance-extension-properties) result
  ;; count = null-terminated
  ;; optional = true
 (layer-name (:pointer :string))
  ;; optional = true
 (property-capacity-input :uint32)
 (property-count-output (:pointer :uint32))
  ;; count = property-capacity-input
  ;; optional = true
 (properties (:pointer (:struct extension-properties))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-out-of-memory
;;          error-limit-reached error-runtime-unavailable error-name-invalid
;;          error-initialization-failed error-extension-not-present
;;          error-api-version-unsupported error-api-layer-not-present)
(defcfun ("xrCreateInstance" create-instance) result
 (create-info (:pointer (:struct instance-create-info)))
 (instance (:pointer instance)))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroyInstance" destroy-instance) result
  ;; externsync = true_with_children
 (instance instance))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost)
(defcfun ("xrResultToString" result-to-string) result
 (instance instance)
 (value result)
  ;; count = +max-result-string-size+ (64)
 (buffer (:pointer :char)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost)
(defcfun ("xrStructureTypeToString" structure-type-to-string) result
 (instance instance)
 (value structure-type)
  ;; count = +max-structure-name-size+ (64)
 (buffer (:pointer :char)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost)
(defcfun ("xrGetInstanceProperties" get-instance-properties) result
 (instance instance)
 (instance-properties (:pointer (:struct instance-properties))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-form-factor-unsupported
;;          error-form-factor-unavailable)
(defcfun ("xrGetSystem" get-system) result
 (instance instance)
 (get-info (:pointer (:struct system-get-info)))
 (system-id (:pointer system-id)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-out-of-memory error-system-invalid)
(defcfun ("xrGetSystemProperties" get-system-properties) result
 (instance instance)
 (system-id system-id)
 (properties (:pointer (:struct system-properties))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-out-of-memory error-limit-reached
;;          error-system-invalid error-initialization-failed
;;          error-graphics-requirements-call-missing error-graphics-device-invalid)
(defcfun ("xrCreateSession" create-session) result
 (instance instance)
 (create-info (:pointer (:struct session-create-info)))
 (session (:pointer session)))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroySession" destroy-session) result
  ;; externsync = true_with_children
 (session session))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroySpace" destroy-space) result
  ;; externsync = true_with_children
 (space space))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-size-insufficient)
(defcfun ("xrEnumerateSwapchainFormats" enumerate-swapchain-formats) result
 (session session)
  ;; optional = true
 (format-capacity-input :uint32)
 (format-count-output (:pointer :uint32))
  ;; count = format-capacity-input
  ;; optional = true
 (formats (:pointer :int64)))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-out-of-memory
;;          error-limit-reached error-swapchain-format-unsupported
;;          error-feature-unsupported)
(defcfun ("xrCreateSwapchain" create-swapchain) result
 (session session)
 (create-info (:pointer (:struct swapchain-create-info)))
 (swapchain (:pointer swapchain)))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroySwapchain" destroy-swapchain) result
  ;; externsync = true_with_children
 (swapchain swapchain))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-size-insufficient)
(defcfun ("xrEnumerateSwapchainImages" enumerate-swapchain-images) result
 (swapchain swapchain)
  ;; optional = true
 (image-capacity-input :uint32)
 (image-count-output (:pointer :uint32))
  ;; count = image-capacity-input
  ;; optional = true
 (images (:pointer (:struct swapchain-image-base-header))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-call-order-invalid)
(defcfun ("xrAcquireSwapchainImage" acquire-swapchain-image) result
 (swapchain swapchain)
  ;; optional = true
 (acquire-info (:pointer (:struct swapchain-image-acquire-info)))
 (index (:pointer :uint32)))

;; success success,xr-session-loss-pending,xr-timeout-expired
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-call-order-invalid)
(defcfun ("xrWaitSwapchainImage" wait-swapchain-image) result
 (swapchain swapchain)
 (wait-info (:pointer (:struct swapchain-image-wait-info))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-call-order-invalid)
(defcfun ("xrReleaseSwapchainImage" release-swapchain-image) result
 (swapchain swapchain)
  ;; optional = true
 (release-info (:pointer (:struct swapchain-image-release-info))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost
;;          error-view-configuration-type-unsupported error-session-running
;;          error-session-not-ready)
(defcfun ("xrBeginSession" begin-session) result
 (session session)
 (begin-info (:pointer (:struct session-begin-info))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-session-not-stopping
;;          error-session-not-running)
(defcfun ("xrEndSession" end-session) result
 (session session))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-session-not-running)
(defcfun ("xrRequestExitSession" request-exit-session) result
 (session session))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-size-insufficient)
(defcfun ("xrEnumerateReferenceSpaces" enumerate-reference-spaces) result
 (session session)
  ;; optional = true
 (space-capacity-input :uint32)
 (space-count-output (:pointer :uint32))
  ;; count = space-capacity-input
  ;; optional = true
 (spaces (:pointer reference-space-type)))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-out-of-memory
;;          error-limit-reached error-reference-space-unsupported
;;          error-pose-invalid)
(defcfun ("xrCreateReferenceSpace" create-reference-space) result
 (session session)
 (create-info (:pointer (:struct reference-space-create-info)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-out-of-memory
;;          error-limit-reached error-pose-invalid error-path-unsupported
;;          error-path-invalid error-action-type-mismatch)
(defcfun ("xrCreateActionSpace" create-action-space) result
 (session session)
 (create-info (:pointer (:struct action-space-create-info)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-time-invalid)
(defcfun ("xrLocateSpace" locate-space) result
 (space space)
 (base-space space)
 (time time)
 (location (:pointer (:struct space-location))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-size-insufficient error-system-invalid)
(defcfun ("xrEnumerateViewConfigurations" enumerate-view-configurations) result
 (instance instance)
 (system-id system-id)
  ;; optional = true
 (view-configuration-type-capacity-input :uint32)
 (view-configuration-type-count-output (:pointer :uint32))
  ;; count = view-configuration-type-capacity-input
  ;; optional = true
 (view-configuration-types (:pointer view-configuration-type)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-size-insufficient
;;          error-view-configuration-type-unsupported error-system-invalid)
(defcfun ("xrEnumerateEnvironmentBlendModes" enumerate-environment-blend-modes) result
 (instance instance)
 (system-id system-id)
 (view-configuration-type view-configuration-type)
  ;; optional = true
 (environment-blend-mode-capacity-input :uint32)
 (environment-blend-mode-count-output (:pointer :uint32))
  ;; count = environment-blend-mode-capacity-input
  ;; optional = true
 (environment-blend-modes (:pointer environment-blend-mode)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-view-configuration-type-unsupported
;;          error-system-invalid)
(defcfun ("xrGetViewConfigurationProperties" get-view-configuration-properties) result
 (instance instance)
 (system-id system-id)
 (view-configuration-type view-configuration-type)
 (configuration-properties (:pointer (:struct view-configuration-properties))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-size-insufficient
;;          error-view-configuration-type-unsupported error-system-invalid)
(defcfun ("xrEnumerateViewConfigurationViews" enumerate-view-configuration-views) result
 (instance instance)
 (system-id system-id)
 (view-configuration-type view-configuration-type)
  ;; optional = true
 (view-capacity-input :uint32)
 (view-count-output (:pointer :uint32))
  ;; count = view-capacity-input
  ;; optional = true
 (views (:pointer (:struct view-configuration-view))))

;; success success,xr-session-loss-pending,xr-frame-discarded
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-session-not-running
;;          error-call-order-invalid)
(defcfun ("xrBeginFrame" begin-frame) result
 (session session)
  ;; optional = true
 (frame-begin-info (:pointer (:struct frame-begin-info)))
  ;; implicit external sync params:
  ;; the pname:session parameter by any other flink:xrBeginFrame or flink:xrEndFrame call
)

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-size-insufficient
;;          error-view-configuration-type-unsupported error-time-invalid)
(defcfun ("xrLocateViews" locate-views) result
 (session session)
 (view-locate-info (:pointer (:struct view-locate-info)))
 (view-state (:pointer (:struct view-state)))
  ;; optional = true
 (view-capacity-input :uint32)
 (view-count-output (:pointer :uint32))
  ;; count = view-capacity-input
  ;; optional = true
 (views (:pointer (:struct view))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-time-invalid
;;          error-swapchain-rect-invalid error-session-not-running
;;          error-pose-invalid error-layer-limit-exceeded error-layer-invalid
;;          error-environment-blend-mode-unsupported error-call-order-invalid)
(defcfun ("xrEndFrame" end-frame) result
 (session session)
 (frame-end-info (:pointer (:struct frame-end-info)))
  ;; implicit external sync params:
  ;; the pname:session parameter by any other flink:xrBeginFrame or flink:xrEndFrame call
)

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-session-not-running)
(defcfun ("xrWaitFrame" wait-frame) result
 (session session)
  ;; optional = true
 (frame-wait-info (:pointer (:struct frame-wait-info)))
 (frame-state (:pointer (:struct frame-state)))
  ;; implicit external sync params:
  ;; the pname:session parameter by any other flink:xrWaitFrame call
)

;; success success,xr-session-loss-pending,xr-session-not-focused
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrApplyHapticFeedback" apply-haptic-feedback) result
 (session session)
 (haptic-action-info (:pointer (:struct haptic-action-info)))
 (haptic-feedback (:pointer (:struct haptic-base-header))))

;; success success,xr-session-loss-pending,xr-session-not-focused
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrStopHapticFeedback" stop-haptic-feedback) result
 (session session)
 (haptic-action-info (:pointer (:struct haptic-action-info))))

;; success success,xr-event-unavailable
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost)
(defcfun ("xrPollEvent" poll-event) result
 (instance instance)
 (event-data (:pointer (:struct event-data-buffer))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-path-format-invalid
;;          error-path-count-exceeded)
(defcfun ("xrStringToPath" string-to-path) result
 (instance instance)
  ;; count = null-terminated
 (path-string (:pointer :string))
 (path (:pointer path)))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-size-insufficient error-path-invalid)
(defcfun ("xrPathToString" path-to-string) result
 (instance instance)
 (path path)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = buffer-capacity-input
  ;; optional = true
 (buffer (:pointer :char)))

;; success success,xr-session-loss-pending,xr-space-bounds-unavailable
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-reference-space-unsupported)
(defcfun ("xrGetReferenceSpaceBoundsRect" get-reference-space-bounds-rect) result
 (session session)
 (reference-space-type reference-space-type)
 (bounds (:pointer (:struct extent-2d-f))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-android-thread-settings-id-invalid-khr
;;          error-android-thread-settings-failure-khr)
(defcfun ("xrSetAndroidApplicationThreadKHR" set-android-application-thread-khr) result
 (session session)
 (thread-type android-thread-type-khr)
 (thread-id :uint32))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached)
(defcfun ("xrCreateSwapchainAndroidSurfaceKHR" create-swapchain-android-surface-khr) result
 (session session)
 (info (:pointer (:struct swapchain-create-info)))
 (swapchain (:pointer swapchain))
 (surface (:pointer j-object)))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrGetActionStateBoolean" get-action-state-boolean) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-boolean))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrGetActionStateFloat" get-action-state-float) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-float))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrGetActionStateVector2f" get-action-state-vector-2f) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-vector-2f))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrGetActionStatePose" get-action-state-pose) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-pose))))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-out-of-memory error-limit-reached
;;          error-path-format-invalid error-name-invalid error-name-duplicated
;;          error-localized-name-invalid error-localized-name-duplicated)
(defcfun ("xrCreateActionSet" create-action-set) result
 (instance instance)
 (create-info (:pointer (:struct action-set-create-info)))
 (action-set (:pointer action-set)))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroyActionSet" destroy-action-set) result
  ;; externsync = true_with_children
 (action-set action-set))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-out-of-memory error-limit-reached
;;          error-path-unsupported error-path-invalid error-path-format-invalid
;;          error-name-invalid error-name-duplicated error-localized-name-invalid
;;          error-localized-name-duplicated error-actionsets-already-attached)
(defcfun ("xrCreateAction" create-action) result
 (action-set action-set)
 (create-info (:pointer (:struct action-create-info)))
 (action (:pointer action)))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroyAction" destroy-action) result
  ;; externsync = true_with_children
 (action action))

;; success success
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-path-unsupported error-path-invalid
;;          error-actionsets-already-attached)
(defcfun ("xrSuggestInteractionProfileBindings" suggest-interaction-profile-bindings) result
 (instance instance)
 (suggested-bindings (:pointer (:struct interaction-profile-suggested-binding))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost
;;          error-actionsets-already-attached)
(defcfun ("xrAttachSessionActionSets" attach-session-action-sets) result
 (session session)
 (attach-info (:pointer (:struct session-action-sets-attach-info))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-actionset-not-attached)
(defcfun ("xrGetCurrentInteractionProfile" get-current-interaction-profile) result
 (session session)
 (top-level-user-path path)
 (interaction-profile (:pointer (:struct interaction-profile-state))))

;; success success,xr-session-loss-pending,xr-session-not-focused
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-path-unsupported
;;          error-path-invalid error-actionset-not-attached)
(defcfun ("xrSyncActions" sync-actions) result
 (session session)
 (sync-info (:pointer (:struct actions-sync-info))))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-size-insufficient
;;          error-path-invalid error-actionset-not-attached)
(defcfun ("xrEnumerateBoundSourcesForAction" enumerate-bound-sources-for-action) result
 (session session)
 (enumerate-info (:pointer (:struct bound-sources-for-action-enumerate-info)))
  ;; optional = true
 (source-capacity-input :uint32)
 (source-count-output (:pointer :uint32))
  ;; count = source-capacity-input
  ;; optional = true
 (sources (:pointer path)))

;; success success,xr-session-loss-pending
;;  errors (error-validation-failure error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-size-insufficient
;;          error-path-unsupported error-path-invalid error-actionset-not-attached)
(defcfun ("xrGetInputSourceLocalizedName" get-input-source-localized-name) result
 (session session)
 (get-info (:pointer (:struct input-source-localized-name-get-info)))
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = buffer-capacity-input
  ;; optional = true
 (buffer (:pointer :char)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-size-insufficient error-system-invalid)
(defcfun ("xrGetVulkanInstanceExtensionsKHR" get-vulkan-instance-extensions-khr) result
 (instance instance)
 (system-id system-id)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = buffer-capacity-input
  ;; optional = true
 (buffer (:pointer :char)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-size-insufficient error-system-invalid)
(defcfun ("xrGetVulkanDeviceExtensionsKHR" get-vulkan-device-extensions-khr) result
 (instance instance)
 (system-id system-id)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = buffer-capacity-input
  ;; optional = true
 (buffer (:pointer :char)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetVulkanGraphicsDeviceKHR" get-vulkan-graphics-device-khr) result
 (instance instance)
 (system-id system-id)
 (vk-instance vk-image)
 (vk-physical-device (:pointer vk-physical-device)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetOpenGLGraphicsRequirementsKHR" get-opengl-graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-opengl-khr))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetOpenGLESGraphicsRequirementsKHR" get-opengl-esgraphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-opengl-es-khr))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetVulkanGraphicsRequirementsKHR" get-vulkan-graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-vulkan-khr))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetD3D11GraphicsRequirementsKHR" get-d3d11graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-d3d11-khr))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetD3D12GraphicsRequirementsKHR" get-d3d12graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-d3d12-khr))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrPerfSettingsSetPerformanceLevelEXT" perf-settings-set-performance-level-ext) result
 (session session)
 (domain perf-settings-domain-ext)
 (level perf-settings-level-ext))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrThermalGetTemperatureTrendEXT" thermal-get-temperature-trend-ext) result
 (session session)
 (domain perf-settings-domain-ext)
 (notification-level (:pointer perf-settings-notification-level-ext))
 (temp-headroom (:pointer :float))
 (temp-slope (:pointer :float)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-out-of-memory)
(defcfun ("xrSetDebugUtilsObjectNameEXT" set-debug-utils-object-name-ext) result
 (instance instance)
  ;; externsync = nameinfo.objecthandle
 (name-info (:pointer (:struct debug-utils-object-name-info-ext))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-out-of-memory error-limit-reached)
(defcfun ("xrCreateDebugUtilsMessengerEXT" create-debug-utils-messenger-ext) result
  ;; externsync = true_with_children
 (instance instance)
 (create-info (:pointer (:struct debug-utils-messenger-create-info-ext)))
 (messenger (:pointer debug-utils-messenger-ext)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroyDebugUtilsMessengerEXT" destroy-debug-utils-messenger-ext) result
  ;; externsync = true
 (messenger debug-utils-messenger-ext)
  ;; implicit external sync params:
  ;; the slink:XrInstance used to create pname:messenger, and all of its child handles
)

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost)
(defcfun ("xrSubmitDebugUtilsMessageEXT" submit-debug-utils-message-ext) result
 (instance instance)
 (message-severity debug-utils-message-severity-flags-ext)
 (message-types debug-utils-message-type-flags-ext)
 (callback-data (:pointer (:struct debug-utils-messenger-callback-data-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrSessionBeginDebugUtilsLabelRegionEXT" session-begin-debug-utils-label-region-ext) result
 (session session)
 (label-info (:pointer (:struct debug-utils-label-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrSessionEndDebugUtilsLabelRegionEXT" session-end-debug-utils-label-region-ext) result
 (session session))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrSessionInsertDebugUtilsLabelEXT" session-insert-debug-utils-label-ext) result
 (session session)
 (label-info (:pointer (:struct debug-utils-label-ext))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-time-invalid)
(defcfun ("xrConvertTimeToWin32PerformanceCounterKHR" convert-time-to-win-32performance-counter-khr) result
 (instance instance)
 (time time)
 (performance-counter (:pointer large-integer)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-time-invalid)
(defcfun ("xrConvertWin32PerformanceCounterToTimeKHR" convert-win-32performance-counter-to-time-khr) result
 (instance instance)
 (performance-counter (:pointer large-integer))
 (time (:pointer time)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-out-of-memory error-limit-reached error-system-invalid)
(defcfun ("xrCreateVulkanInstanceKHR" create-vulkan-instance-khr) result
 (instance instance)
 (create-info (:pointer (:struct vulkan-instance-create-info-khr)))
 (vulkan-instance (:pointer vk-image))
 (vulkan-result (:pointer vk-result)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-out-of-memory error-limit-reached error-system-invalid)
(defcfun ("xrCreateVulkanDeviceKHR" create-vulkan-device-khr) result
 (instance instance)
 (create-info (:pointer (:struct vulkan-device-create-info-khr)))
 (vulkan-device (:pointer vk-device))
 (vulkan-result (:pointer vk-result)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetVulkanGraphicsDevice2KHR" get-vulkan-graphics-device-2-khr) result
 (instance instance)
 (get-info (:pointer (:struct vulkan-graphics-device-get-info-khr)))
 (vulkan-physical-device (:pointer vk-physical-device)))

;; alias "xrGetVulkanGraphicsRequirements2KHR" -> "xrGetVulkanGraphicsRequirementsKHR"
;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-system-invalid)
(defcfun ("xrGetVulkanGraphicsRequirements2KHR" get-vulkan-graphics-requirements-2-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-vulkan-khr))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-time-invalid)
(defcfun ("xrConvertTimeToTimespecTimeKHR" convert-time-to-timespec-time-khr) result
 (instance instance)
 (time time)
 (timespec-time (:pointer timespec)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-time-invalid)
(defcfun ("xrConvertTimespecTimeToTimeKHR" convert-timespec-time-to-time-khr) result
 (instance instance)
 (timespec-time (:pointer timespec))
 (time (:pointer time)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-size-insufficient
;;          error-view-configuration-type-unsupported)
(defcfun ("xrGetVisibilityMaskKHR" get-visibility-mask-khr) result
 (session session)
 (view-configuration-type view-configuration-type)
 (view-index :uint32)
 (visibility-mask-type visibility-mask-type-khr)
 (visibility-mask (:pointer (:struct visibility-mask-khr))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached error-time-invalid
;;          error-pose-invalid error-create-spatial-anchor-failed-msft)
(defcfun ("xrCreateSpatialAnchorMSFT" create-spatial-anchor-msft) result
 (session session)
 (create-info (:pointer (:struct spatial-anchor-create-info-msft)))
 (anchor (:pointer spatial-anchor-msft)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached error-pose-invalid)
(defcfun ("xrCreateSpatialAnchorSpaceMSFT" create-spatial-anchor-space-msft) result
 (session session)
 (create-info (:pointer (:struct spatial-anchor-space-create-info-msft)))
 (space (:pointer space)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroySpatialAnchorMSFT" destroy-spatial-anchor-msft) result
  ;; externsync = true_with_children
 (anchor spatial-anchor-msft))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-path-unsupported error-path-invalid)
(defcfun ("xrSetInputDeviceActiveEXT" set-input-device-active-ext) result
 (session session)
 (interaction-profile path)
 (top-level-path path)
 (is-active bool-32))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-path-unsupported error-path-invalid)
(defcfun ("xrSetInputDeviceStateBoolEXT" set-input-device-state-bool-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (state bool-32))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-path-unsupported error-path-invalid)
(defcfun ("xrSetInputDeviceStateFloatEXT" set-input-device-state-float-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (state :float))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-path-unsupported error-path-invalid)
(defcfun ("xrSetInputDeviceStateVector2fEXT" set-input-device-state-vector-2f-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (state (:struct vector-2f)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-pose-invalid error-path-unsupported error-path-invalid)
(defcfun ("xrSetInputDeviceLocationEXT" set-input-device-location-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (space space)
 (pose (:struct pose-f)))

;; success success
;;  errors (error-function-unsupported error-validation-failure)
(defcfun ("xrInitializeLoaderKHR" initialize-loader-khr) result
 (loader-init-info (:pointer (:struct loader-init-info-base-header-khr))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached error-pose-invalid)
(defcfun ("xrCreateSpatialGraphNodeSpaceMSFT" create-spatial-graph-node-space-msft) result
 (session session)
 (create-info (:pointer (:struct spatial-graph-node-space-create-info-msft)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-time-invalid error-pose-invalid)
(defcfun ("xrTryCreateSpatialGraphStaticNodeBindingMSFT" try-create-spatial-graph-static-node-binding-msft) result
 (session session)
 (create-info (:pointer
               (:struct spatial-graph-static-node-binding-create-info-msft)))
 (node-binding (:pointer spatial-graph-node-binding-msft)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroySpatialGraphNodeBindingMSFT" destroy-spatial-graph-node-binding-msft) result
  ;; externsync = true_with_children
 (node-binding spatial-graph-node-binding-msft))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory)
(defcfun ("xrGetSpatialGraphNodeBindingPropertiesMSFT" get-spatial-graph-node-binding-properties-msft) result
 (node-binding spatial-graph-node-binding-msft)
  ;; optional = true
 (get-info (:pointer
            (:struct spatial-graph-node-binding-properties-get-info-msft)))
 (properties (:pointer (:struct spatial-graph-node-binding-properties-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreateHandTrackerEXT" create-hand-tracker-ext) result
 (session session)
 (create-info (:pointer (:struct hand-tracker-create-info-ext)))
 (hand-tracker (:pointer hand-tracker-ext)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroyHandTrackerEXT" destroy-hand-tracker-ext) result
  ;; externsync = true_with_children
 (hand-tracker hand-tracker-ext))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-time-invalid)
(defcfun ("xrLocateHandJointsEXT" locate-hand-joints-ext) result
 (hand-tracker hand-tracker-ext)
 (locate-info (:pointer (:struct hand-joints-locate-info-ext)))
 (locations (:pointer (:struct hand-joint-locations-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreateFaceTrackerFB" create-face-tracker-fb) result
 (session session)
 (create-info (:pointer (:struct face-tracker-create-info-fb)))
 (face-tracker (:pointer face-tracker-fb)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroyFaceTrackerFB" destroy-face-tracker-fb) result
  ;; externsync = true_with_children
 (face-tracker face-tracker-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-time-invalid)
(defcfun ("xrGetFaceExpressionWeightsFB" get-face-expression-weights-fb) result
 (face-tracker face-tracker-fb)
 (expression-info (:pointer (:struct face-expression-info-fb)))
 (expression-weights (:pointer (:struct face-expression-weights-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreateBodyTrackerFB" create-body-tracker-fb) result
 (session session)
 (create-info (:pointer (:struct body-tracker-create-info-fb)))
 (body-tracker (:pointer body-tracker-fb)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroyBodyTrackerFB" destroy-body-tracker-fb) result
  ;; externsync = true_with_children
 (body-tracker body-tracker-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-time-invalid)
(defcfun ("xrLocateBodyJointsFB" locate-body-joints-fb) result
 (body-tracker body-tracker-fb)
 (locate-info (:pointer (:struct body-joints-locate-info-fb)))
 (locations (:pointer (:struct body-joint-locations-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrGetBodySkeletonFB" get-body-skeleton-fb) result
 (body-tracker body-tracker-fb)
 (skeleton (:pointer (:struct body-skeleton-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreateEyeTrackerFB" create-eye-tracker-fb) result
 (session session)
 (create-info (:pointer (:struct eye-tracker-create-info-fb)))
 (eye-tracker (:pointer eye-tracker-fb)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroyEyeTrackerFB" destroy-eye-tracker-fb) result
  ;; externsync = true_with_children
 (eye-tracker eye-tracker-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-time-invalid)
(defcfun ("xrGetEyeGazesFB" get-eye-gazes-fb) result
 (eye-tracker eye-tracker-fb)
 (gaze-info (:pointer (:struct eye-gazes-info-fb)))
 (eye-gazes (:pointer (:struct eye-gazes-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-pose-invalid error-feature-unsupported)
(defcfun ("xrCreateHandMeshSpaceMSFT" create-hand-mesh-space-msft) result
 (hand-tracker hand-tracker-ext)
 (create-info (:pointer (:struct hand-mesh-space-create-info-msft)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-size-insufficient error-time-invalid
;;          error-feature-unsupported)
(defcfun ("xrUpdateHandMeshMSFT" update-hand-mesh-msft) result
 (hand-tracker hand-tracker-ext)
 (update-info (:pointer (:struct hand-mesh-update-info-msft)))
 (hand-mesh (:pointer (:struct hand-mesh-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-path-unsupported error-path-invalid
;;          error-controller-model-key-invalid-msft)
(defcfun ("xrGetControllerModelKeyMSFT" get-controller-model-key-msft) result
 (session session)
 (top-level-user-path path)
 (controller-model-key-state (:pointer
                              (:struct controller-model-key-state-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-size-insufficient
;;          error-controller-model-key-invalid-msft)
(defcfun ("xrLoadControllerModelMSFT" load-controller-model-msft) result
 (session session)
 (model-key controller-model-key-msft)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = buffer-capacity-input
  ;; optional = true
 (buffer (:pointer :uint8)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-controller-model-key-invalid-msft)
(defcfun ("xrGetControllerModelPropertiesMSFT" get-controller-model-properties-msft) result
 (session session)
 (model-key controller-model-key-msft)
 (properties (:pointer (:struct controller-model-properties-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-controller-model-key-invalid-msft)
(defcfun ("xrGetControllerModelStateMSFT" get-controller-model-state-msft) result
 (session session)
 (model-key controller-model-key-msft)
 (state (:pointer (:struct controller-model-state-msft))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-out-of-memory error-size-insufficient error-system-invalid)
(defcfun ("xrEnumerateSceneComputeFeaturesMSFT" enumerate-scene-compute-features-msft) result
 (instance instance)
 (system-id system-id)
  ;; optional = true
 (feature-capacity-input :uint32)
 (feature-count-output (:pointer :uint32))
  ;; count = feature-capacity-input
  ;; optional = true
 (features (:pointer scene-compute-feature-msft)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached)
(defcfun ("xrCreateSceneObserverMSFT" create-scene-observer-msft) result
 (session session)
  ;; optional = true
 (create-info (:pointer (:struct scene-observer-create-info-msft)))
 (scene-observer (:pointer scene-observer-msft)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroySceneObserverMSFT" destroy-scene-observer-msft) result
  ;; externsync = true_with_children
 (scene-observer scene-observer-msft))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-compute-new-scene-not-completed-msft)
(defcfun ("xrCreateSceneMSFT" create-scene-msft) result
 (scene-observer scene-observer-msft)
  ;; optional = true
 (create-info (:pointer (:struct scene-create-info-msft)))
 (scene (:pointer scene-msft)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroySceneMSFT" destroy-scene-msft) result
  ;; externsync = true_with_children
 (scene scene-msft))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-time-invalid
;;          error-scene-compute-feature-incompatible-msft
;;          error-scene-compute-consistency-mismatch-msft error-pose-invalid
;;          error-compute-new-scene-not-completed-msft)
(defcfun ("xrComputeNewSceneMSFT" compute-new-scene-msft) result
 (scene-observer scene-observer-msft)
 (compute-info (:pointer (:struct new-scene-compute-info-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory)
(defcfun ("xrGetSceneComputeStateMSFT" get-scene-compute-state-msft) result
 (scene-observer scene-observer-msft)
 (state (:pointer scene-compute-state-msft)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-size-insufficient
;;          error-scene-component-type-mismatch-msft)
(defcfun ("xrGetSceneComponentsMSFT" get-scene-components-msft) result
 (scene scene-msft)
 (get-info (:pointer (:struct scene-components-get-info-msft)))
 (components (:pointer (:struct scene-components-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-size-insufficient
;;          error-time-invalid)
(defcfun ("xrLocateSceneComponentsMSFT" locate-scene-components-msft) result
 (scene scene-msft)
 (locate-info (:pointer (:struct scene-components-locate-info-msft)))
 (locations (:pointer (:struct scene-component-locations-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory
;;          error-scene-mesh-buffer-id-invalid-msft
;;          error-scene-component-id-invalid-msft)
(defcfun ("xrGetSceneMeshBuffersMSFT" get-scene-mesh-buffers-msft) result
 (scene scene-msft)
 (get-info (:pointer (:struct scene-mesh-buffers-get-info-msft)))
 (buffers (:pointer (:struct scene-mesh-buffers-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory
;;          error-compute-new-scene-not-completed-msft)
(defcfun ("xrDeserializeSceneMSFT" deserialize-scene-msft) result
 (scene-observer scene-observer-msft)
 (deserialize-info (:pointer (:struct scene-deserialize-info-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-size-insufficient
;;          error-scene-component-id-invalid-msft)
(defcfun ("xrGetSerializedSceneFragmentDataMSFT" get-serialized-scene-fragment-data-msft) result
 (scene scene-msft)
 (get-info (:pointer (:struct serialized-scene-fragment-data-get-info-msft)))
  ;; optional = true
 (count-input :uint32)
 (read-output (:pointer :uint32))
  ;; count = count-input
  ;; optional = true
 (buffer (:pointer :uint8)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-size-insufficient)
(defcfun ("xrEnumerateDisplayRefreshRatesFB" enumerate-display-refresh-rates-fb) result
 (session session)
  ;; optional = true
 (display-refresh-rate-capacity-input :uint32)
 (display-refresh-rate-count-output (:pointer :uint32))
  ;; count = display-refresh-rate-capacity-input
  ;; optional = true
 (display-refresh-rates (:pointer :float)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrGetDisplayRefreshRateFB" get-display-refresh-rate-fb) result
 (session session)
 (display-refresh-rate (:pointer :float)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported
;;          error-display-refresh-rate-unsupported-fb)
(defcfun ("xrRequestDisplayRefreshRateFB" request-display-refresh-rate-fb) result
 (session session)
 (display-refresh-rate :float))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached)
(defcfun ("xrCreateSpatialAnchorFromPerceptionAnchorMSFT" create-spatial-anchor-from-perception-anchor-msft) result
 (session session)
 (perception-anchor (:pointer i-unknown))
 (anchor (:pointer spatial-anchor-msft)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory)
(defcfun ("xrTryGetPerceptionAnchorFromSpatialAnchorMSFT" try-get-perception-anchor-from-spatial-anchor-msft) result
 (session session)
 (anchor spatial-anchor-msft)
 (perception-anchor (:pointer (:pointer i-unknown))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrUpdateSwapchainFB" update-swapchain-fb) result
 (swapchain swapchain)
 (state (:pointer (:struct swapchain-state-base-header-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrGetSwapchainStateFB" get-swapchain-state-fb) result
 (swapchain swapchain)
 (state (:pointer (:struct swapchain-state-base-header-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-size-insufficient)
(defcfun ("xrEnumerateColorSpacesFB" enumerate-color-spaces-fb) result
 (session session)
  ;; optional = true
 (color-space-capacity-input :uint32)
 (color-space-count-output (:pointer :uint32))
  ;; count = color-space-capacity-input
  ;; optional = true
 (color-spaces (:pointer color-space-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported
;;          error-color-space-unsupported-fb)
(defcfun ("xrSetColorSpaceFB" set-color-space-fb) result
 (session session)
 (colorspace color-space-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached)
(defcfun ("xrCreateFoveationProfileFB" create-foveation-profile-fb) result
 (session session)
 (create-info (:pointer (:struct foveation-profile-create-info-fb)))
 (profile (:pointer foveation-profile-fb)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid)
(defcfun ("xrDestroyFoveationProfileFB" destroy-foveation-profile-fb) result
  ;; externsync = true_with_children
 (profile foveation-profile-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrGetFoveationEyeTrackedStateMETA" get-foveation-eye-tracked-state-meta) result
 (session session)
 (foveation-state (:pointer (:struct foveation-eye-tracked-state-meta))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-size-insufficient error-feature-unsupported)
(defcfun ("xrGetHandMeshFB" get-hand-mesh-fb) result
 (hand-tracker hand-tracker-ext)
 (mesh (:pointer (:struct hand-tracking-mesh-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory)
(defcfun ("xrEnumerateRenderModelPathsFB" enumerate-render-model-paths-fb) result
 (session session)
  ;; optional = true
 (path-capacity-input :uint32)
 (path-count-output (:pointer :uint32))
  ;; count = path-capacity-input
  ;; optional = true
 (paths (:pointer (:struct render-model-path-info-fb))))

;; success success,xr-session-loss-pending,xr-render-model-unavailable-fb
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-path-unsupported error-path-invalid
;;          error-call-order-invalid)
(defcfun ("xrGetRenderModelPropertiesFB" get-render-model-properties-fb) result
 (session session)
 (path path)
 (properties (:pointer (:struct render-model-properties-fb))))

;; success success,xr-session-loss-pending,xr-render-model-unavailable-fb
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-render-model-key-invalid-fb)
(defcfun ("xrLoadRenderModelFB" load-render-model-fb) result
 (session session)
 (info (:pointer (:struct render-model-load-info-fb)))
 (buffer (:pointer (:struct render-model-buffer-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrQuerySystemTrackedKeyboardFB" query-system-tracked-keyboard-fb) result
 (session session)
 (query-info (:pointer (:struct keyboard-tracking-query-fb)))
 (keyboard (:pointer (:struct keyboard-tracking-description-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreateKeyboardSpaceFB" create-keyboard-space-fb) result
 (session session)
 (create-info (:pointer (:struct keyboard-space-create-info-fb)))
 (keyboard-space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-feature-unsupported)
(defcfun ("xrSetEnvironmentDepthEstimationVARJO" set-environment-depth-estimation-varjo) result
 (session session)
 (enabled bool-32))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-size-insufficient error-view-configuration-type-unsupported
;;          error-system-invalid)
(defcfun ("xrEnumerateReprojectionModesMSFT" enumerate-reprojection-modes-msft) result
 (instance instance)
 (system-id system-id)
 (view-configuration-type view-configuration-type)
  ;; optional = true
 (mode-capacity-input :uint32)
 (mode-count-output (:pointer :uint32))
  ;; count = mode-capacity-input
  ;; optional = true
 (modes (:pointer reprojection-mode-msft)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-feature-unsupported)
(defcfun ("xrGetAudioOutputDeviceGuidOculus" get-audio-output-device-guid-oculus) result
 (instance instance)
  ;; count = +max-audio-device-str-size-oculus+ (128)
 (buffer (:pointer :uint16)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-feature-unsupported)
(defcfun ("xrGetAudioInputDeviceGuidOculus" get-audio-input-device-guid-oculus) result
 (instance instance)
  ;; count = +max-audio-device-str-size-oculus+ (128)
 (buffer (:pointer :uint16)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-time-invalid error-pose-invalid error-feature-unsupported)
(defcfun ("xrCreateSpatialAnchorFB" create-spatial-anchor-fb) result
 (session session)
 (info (:pointer (:struct spatial-anchor-create-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrGetSpaceUuidFB" get-space-uuid-fb) result
 (space space)
 (uuid (:pointer (:struct uuid-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrEnumerateSpaceSupportedComponentsFB" enumerate-space-supported-components-fb) result
 (space space)
  ;; optional = true
 (component-type-capacity-input :uint32)
 (component-type-count-output (:pointer :uint32))
  ;; count = component-type-capacity-input
  ;; optional = true
 (component-types (:pointer space-component-type-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-status-pending-fb
;;          error-space-component-status-already-set-fb
;;          error-space-component-not-supported-fb error-feature-unsupported)
(defcfun ("xrSetSpaceComponentStatusFB" set-space-component-status-fb) result
 (space space)
 (info (:pointer (:struct space-component-status-set-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-supported-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceComponentStatusFB" get-space-component-status-fb) result
 (space space)
 (component-type space-component-type-fb)
 (status (:pointer (:struct space-component-status-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-insufficient-resources-passthrough-fb error-feature-unsupported)
(defcfun ("xrCreateTriangleMeshFB" create-triangle-mesh-fb) result
 (session session)
 (create-info (:pointer (:struct triangle-mesh-create-info-fb)))
 (out-triangle-mesh (:pointer triangle-mesh-fb)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-feature-unsupported)
(defcfun ("xrDestroyTriangleMeshFB" destroy-triangle-mesh-fb) result
  ;; externsync = true_with_children
 (mesh triangle-mesh-fb)
  ;; implicit external sync params:
  ;; the buffers returned from calls to flink:xrTriangleMeshGetVertexBufferFB and flink:xrTriangleMeshGetIndexBufferFB on pname:mesh
)

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrTriangleMeshGetVertexBufferFB" triangle-mesh-get-vertex-buffer-fb) result
 (mesh triangle-mesh-fb)
 (out-vertex-buffer (:pointer (:pointer (:struct vector-3f)))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrTriangleMeshGetIndexBufferFB" triangle-mesh-get-index-buffer-fb) result
 (mesh triangle-mesh-fb)
 (out-index-buffer (:pointer (:pointer :uint32))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported error-call-order-invalid)
(defcfun ("xrTriangleMeshBeginUpdateFB" triangle-mesh-begin-update-fb) result
 (mesh triangle-mesh-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported error-call-order-invalid)
(defcfun ("xrTriangleMeshEndUpdateFB" triangle-mesh-end-update-fb) result
 (mesh triangle-mesh-fb)
 (vertex-count :uint32)
 (triangle-count :uint32))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported error-call-order-invalid)
(defcfun ("xrTriangleMeshBeginVertexBufferUpdateFB" triangle-mesh-begin-vertex-buffer-update-fb) result
 (mesh triangle-mesh-fb)
 (out-vertex-count (:pointer :uint32)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported error-call-order-invalid)
(defcfun ("xrTriangleMeshEndVertexBufferUpdateFB" triangle-mesh-end-vertex-buffer-update-fb) result
 (mesh triangle-mesh-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-unknown-passthrough-fb error-not-permitted-passthrough-fb
;;          error-feature-unsupported error-feature-already-created-passthrough-fb)
(defcfun ("xrCreatePassthroughFB" create-passthrough-fb) result
 (session session)
 (create-info (:pointer (:struct passthrough-create-info-fb)))
 (out-passthrough (:pointer passthrough-fb)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-feature-unsupported)
(defcfun ("xrDestroyPassthroughFB" destroy-passthrough-fb) result
  ;; externsync = true_with_children
 (passthrough passthrough-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-unexpected-state-passthrough-fb
;;          error-feature-unsupported)
(defcfun ("xrPassthroughStartFB" passthrough-start-fb) result
 (passthrough passthrough-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-unexpected-state-passthrough-fb
;;          error-feature-unsupported)
(defcfun ("xrPassthroughPauseFB" passthrough-pause-fb) result
 (passthrough passthrough-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-unknown-passthrough-fb
;;          error-insufficient-resources-passthrough-fb error-feature-unsupported
;;          error-feature-required-passthrough-fb)
(defcfun ("xrCreatePassthroughLayerFB" create-passthrough-layer-fb) result
 (session session)
 (create-info (:pointer (:struct passthrough-layer-create-info-fb)))
 (out-layer (:pointer passthrough-layer-fb)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-feature-unsupported)
(defcfun ("xrDestroyPassthroughLayerFB" destroy-passthrough-layer-fb) result
  ;; externsync = true_with_children
 (layer passthrough-layer-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-unexpected-state-passthrough-fb
;;          error-feature-unsupported)
(defcfun ("xrPassthroughLayerPauseFB" passthrough-layer-pause-fb) result
 (layer passthrough-layer-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-unexpected-state-passthrough-fb
;;          error-feature-unsupported)
(defcfun ("xrPassthroughLayerResumeFB" passthrough-layer-resume-fb) result
 (layer passthrough-layer-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrPassthroughLayerSetStyleFB" passthrough-layer-set-style-fb) result
 (layer passthrough-layer-fb)
 (style (:pointer (:struct passthrough-style-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-pose-invalid error-insufficient-resources-passthrough-fb
;;          error-feature-unsupported)
(defcfun ("xrCreateGeometryInstanceFB" create-geometry-instance-fb) result
 (session session)
 (create-info (:pointer (:struct geometry-instance-create-info-fb)))
 (out-geometry-instance (:pointer geometry-instance-fb)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-feature-unsupported)
(defcfun ("xrDestroyGeometryInstanceFB" destroy-geometry-instance-fb) result
  ;; externsync = true_with_children
 (instance geometry-instance-fb))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-time-invalid error-pose-invalid
;;          error-feature-unsupported)
(defcfun ("xrGeometryInstanceSetTransformFB" geometry-instance-set-transform-fb) result
 (instance geometry-instance-fb)
 (transformation (:pointer (:struct geometry-instance-transform-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrQuerySpacesFB" query-spaces-fb) result
 (session session)
 (info (:pointer (:struct space-query-info-base-header-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrRetrieveSpaceQueryResultsFB" retrieve-space-query-results-fb) result
 (session session)
 (request-id async-request-id-fb)
 (results (:pointer (:struct space-query-results-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrSaveSpaceFB" save-space-fb) result
 (session session)
 (info (:pointer (:struct space-save-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrEraseSpaceFB" erase-space-fb) result
 (session session)
 (info (:pointer (:struct space-erase-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-network-timeout-fb
;;          error-space-network-request-failed-fb
;;          error-space-mapping-insufficient-fb error-space-localization-failed-fb
;;          error-space-component-not-enabled-fb
;;          error-space-cloud-storage-disabled-fb error-feature-unsupported)
(defcfun ("xrSaveSpaceListFB" save-space-list-fb) result
 (session session)
 (info (:pointer (:struct space-list-save-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-network-timeout-fb
;;          error-space-network-request-failed-fb
;;          error-space-mapping-insufficient-fb error-space-localization-failed-fb
;;          error-space-component-not-enabled-fb
;;          error-space-cloud-storage-disabled-fb error-feature-unsupported)
(defcfun ("xrShareSpacesFB" share-spaces-fb) result
 (session session)
 (info (:pointer (:struct space-share-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceContainerFB" get-space-container-fb) result
 (session session)
 (space space)
 (space-container-output (:pointer (:struct space-container-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceBoundingBox2DFB" get-space-bounding-box-2dfb) result
 (session session)
 (space space)
 (bounding-box-2doutput (:pointer (:struct rect-2d-f))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceBoundingBox3DFB" get-space-bounding-box-3dfb) result
 (session session)
 (space space)
 (bounding-box-3doutput (:pointer (:struct rect-3df-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceSemanticLabelsFB" get-space-semantic-labels-fb) result
 (session session)
 (space space)
 (semantic-labels-output (:pointer (:struct semantic-labels-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceBoundary2DFB" get-space-boundary-2dfb) result
 (session session)
 (space space)
 (boundary-2doutput (:pointer (:struct boundary-2dfb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-space-component-not-enabled-fb
;;          error-feature-unsupported)
(defcfun ("xrGetSpaceRoomLayoutFB" get-space-room-layout-fb) result
 (session session)
 (space space)
 (room-layout-output (:pointer (:struct room-layout-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrRequestSceneCaptureFB" request-scene-capture-fb) result
 (session session)
 (info (:pointer (:struct scene-capture-request-info-fb)))
 (request-id (:pointer async-request-id-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrPassthroughLayerSetKeyboardHandsIntensityFB" passthrough-layer-set-keyboard-hands-intensity-fb) result
 (layer passthrough-layer-fb)
 (intensity (:pointer (:struct passthrough-keyboard-hands-intensity-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached)
(defcfun ("xrCreateSpatialAnchorStoreConnectionMSFT" create-spatial-anchor-store-connection-msft) result
 (session session)
 (spatial-anchor-store (:pointer spatial-anchor-store-connection-msft)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid error-out-of-memory)
(defcfun ("xrDestroySpatialAnchorStoreConnectionMSFT" destroy-spatial-anchor-store-connection-msft) result
  ;; externsync = true_with_children
 (spatial-anchor-store spatial-anchor-store-connection-msft))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-spatial-anchor-name-invalid-msft)
(defcfun ("xrPersistSpatialAnchorMSFT" persist-spatial-anchor-msft) result
 (spatial-anchor-store spatial-anchor-store-connection-msft)
 (spatial-anchor-persistence-info (:pointer
                                   (:struct
                                    spatial-anchor-persistence-info-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-size-insufficient)
(defcfun ("xrEnumeratePersistedSpatialAnchorNamesMSFT" enumerate-persisted-spatial-anchor-names-msft) result
 (spatial-anchor-store spatial-anchor-store-connection-msft)
  ;; optional = true
 (spatial-anchor-names-capacity-input :uint32)
  ;; optional = true
 (spatial-anchor-names-count-output (:pointer :uint32))
  ;; count = spatial-anchor-names-capacity-input
  ;; optional = true
 (persisted-anchor-names (:pointer
                          (:struct spatial-anchor-persistence-name-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached
;;          error-spatial-anchor-name-not-found-msft
;;          error-spatial-anchor-name-invalid-msft)
(defcfun ("xrCreateSpatialAnchorFromPersistedNameMSFT" create-spatial-anchor-from-persisted-name-msft) result
 (session session)
 (spatial-anchor-create-info (:pointer
                              (:struct
                               spatial-anchor-from-persisted-anchor-create-info-msft)))
 (spatial-anchor (:pointer spatial-anchor-msft)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-spatial-anchor-name-not-found-msft
;;          error-spatial-anchor-name-invalid-msft)
(defcfun ("xrUnpersistSpatialAnchorMSFT" unpersist-spatial-anchor-msft) result
 (spatial-anchor-store spatial-anchor-store-connection-msft)
 (spatial-anchor-persistence-name (:pointer
                                   (:struct
                                    spatial-anchor-persistence-name-msft))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory)
(defcfun ("xrClearSpatialAnchorStoreMSFT" clear-spatial-anchor-store-msft) result
 (spatial-anchor-store spatial-anchor-store-connection-msft))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreateFacialTrackerHTC" create-facial-tracker-htc) result
 (session session)
 (create-info (:pointer (:struct facial-tracker-create-info-htc)))
 (facial-tracker (:pointer facial-tracker-htc)))

;; success success
;;  errors (error-function-unsupported error-handle-invalid)
(defcfun ("xrDestroyFacialTrackerHTC" destroy-facial-tracker-htc) result
  ;; externsync = true_with_children
 (facial-tracker facial-tracker-htc))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-time-invalid)
(defcfun ("xrGetFacialExpressionsHTC" get-facial-expressions-htc) result
 (facial-tracker facial-tracker-htc)
 (facial-expressions (:pointer (:struct facial-expressions-htc))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-feature-unsupported)
(defcfun ("xrCreatePassthroughHTC" create-passthrough-htc) result
 (session session)
 (create-info (:pointer (:struct passthrough-create-info-htc)))
 (passthrough (:pointer passthrough-htc)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid)
(defcfun ("xrDestroyPassthroughHTC" destroy-passthrough-htc) result
  ;; externsync = true_with_children
 (passthrough passthrough-htc))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-size-insufficient)
(defcfun ("xrEnumerateViveTrackerPathsHTCX" enumerate-vive-tracker-paths-htcx) result
 (instance instance)
  ;; optional = true
 (path-capacity-input :uint32)
 (path-count-output (:pointer :uint32))
  ;; count = path-capacity-input
  ;; optional = true
 (paths (:pointer (:struct vive-tracker-paths-htcx))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-feature-unsupported)
(defcfun ("xrSetMarkerTrackingVARJO" set-marker-tracking-varjo) result
 (session session)
 (enabled bool-32))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-marker-id-invalid-varjo
;;          error-feature-unsupported)
(defcfun ("xrSetMarkerTrackingTimeoutVARJO" set-marker-tracking-timeout-varjo) result
 (session session)
 (marker-id :uint64)
 (timeout duration))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-marker-id-invalid-varjo
;;          error-feature-unsupported)
(defcfun ("xrSetMarkerTrackingPredictionVARJO" set-marker-tracking-prediction-varjo) result
 (session session)
 (marker-id :uint64)
 (enabled bool-32))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid
;;          error-instance-lost error-session-lost error-marker-not-tracked-varjo
;;          error-marker-id-invalid-varjo error-feature-unsupported)
(defcfun ("xrGetMarkerSizeVARJO" get-marker-size-varjo) result
 (session session)
 (marker-id :uint64)
 (size (:pointer (:struct extent-2d-f))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-out-of-memory error-limit-reached error-pose-invalid
;;          error-marker-id-invalid-varjo error-feature-unsupported)
(defcfun ("xrCreateMarkerSpaceVARJO" create-marker-space-varjo) result
 (session session)
 (create-info (:pointer (:struct marker-space-create-info-varjo)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrSetDigitalLensControlALMALENCE" set-digital-lens-control-almalence) result
 (session session)
 (digital-lens-control (:pointer (:struct digital-lens-control-almalence))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-feature-unsupported)
(defcfun ("xrSetViewOffsetVARJO" set-view-offset-varjo) result
 (session session)
 (offset :float))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-size-insufficient)
(defcfun ("xrEnumerateExternalCamerasOCULUS" enumerate-external-cameras-oculus) result
 (session session)
  ;; optional = true
 (camera-capacity-input :uint32)
 (camera-count-output (:pointer :uint32))
  ;; count = camera-capacity-input
  ;; optional = true
 (cameras (:pointer (:struct external-camera-oculus))))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-size-insufficient)
(defcfun ("xrEnumeratePerformanceMetricsCounterPathsMETA" enumerate-performance-metrics-counter-paths-meta) result
 (instance instance)
  ;; optional = true
 (counter-path-capacity-input :uint32)
 (counter-path-count-output (:pointer :uint32))
  ;; count = counter-path-capacity-input
  ;; optional = true
 (counter-paths (:pointer path)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrSetPerformanceMetricsStateMETA" set-performance-metrics-state-meta) result
 (session session)
 (state (:pointer (:struct performance-metrics-state-meta))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost)
(defcfun ("xrGetPerformanceMetricsStateMETA" get-performance-metrics-state-meta) result
 (session session)
 (state (:pointer (:struct performance-metrics-state-meta))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-path-unsupported error-path-invalid)
(defcfun ("xrQueryPerformanceMetricsCounterMETA" query-performance-metrics-counter-meta) result
 (session session)
 (counter-path path)
 (counter (:pointer (:struct performance-metrics-counter-meta))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-limit-reached)
(defcfun ("xrApplyFoveationHTC" apply-foveation-htc) result
 (session session)
 (apply-info (:pointer (:struct foveation-apply-info-htc))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached
;;          error-pose-invalid)
(defcfun ("xrCreateSpaceFromCoordinateFrameUIDML" create-space-from-coordinate-frame-uidml) result
 (session session)
 (create-info (:pointer (:struct coordinate-space-create-info-ml)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-instance-lost error-session-lost
;;          error-path-unsupported error-path-invalid error-action-type-mismatch
;;          error-actionset-not-attached)
(defcfun ("xrGetDeviceSampleRateFB" get-device-sample-rate-fb) result
 (session session)
 (haptic-action-info (:pointer (:struct haptic-action-info)))
 (device-sample-rate (:pointer (:struct device-pcm-sample-rate-get-info-fb))))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-hint-already-set-qcom)
(defcfun ("xrSetTrackingOptimizationSettingsHintQCOM" set-tracking-optimization-settings-hint-qcom) result
 (session session)
 (domain tracking-optimization-settings-domain-qcom)
 (hint tracking-optimization-settings-hint-qcom))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost error-out-of-memory error-limit-reached)
(defcfun ("xrCreateSpaceUserFB" create-space-user-fb) result
 (session session)
 (info (:pointer (:struct space-user-create-info-fb)))
 (user (:pointer space-user-fb)))

;; success success,xr-session-loss-pending
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrGetSpaceUserIdFB" get-space-user-id-fb) result
 (user space-user-fb)
 (user-id (:pointer space-user-id-fb)))

;; success success
;;  errors (error-function-unsupported error-runtime-failure error-handle-invalid)
(defcfun ("xrDestroySpaceUserFB" destroy-space-user-fb) result
  ;; externsync = true_with_children
 (user space-user-fb))

;; success success,xr-session-loss-pending,xr-session-not-focused
;;  errors (error-function-unsupported error-validation-failure
;;          error-runtime-failure error-handle-invalid error-instance-lost
;;          error-session-lost)
(defcfun ("xrApplyForceFeedbackCurlMNDX" apply-force-feedback-curl-mndx) result
 (hand-tracker hand-tracker-ext)
 (locations (:pointer (:struct force-feedback-curl-apply-locations-mndx))))

