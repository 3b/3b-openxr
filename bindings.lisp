(in-package #:3b-openxr-bindings)
;; Copyright (c) 2017-2020 The Khronos Group Inc.
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and/or associated documentation files (the
;; "Materials"), to deal in the Materials without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Materials, and to
;; permit persons to whom the Materials are furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Materials.
;; 
;; THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
;; 
;; ------------------------------------------------------------------------
;; 
;; This file, xr.xml, is the OpenXR API Registry. It is a critically important
;; and normative part of the OpenXR Specification, including a canonical
;; machine-readable definition of the API, parameter and member validation
;; language incorporated into the Specification and reference pages, and other
;; material which is registered by Khronos, such as tags used by extension and
;; layer authors. The only authoritative version of xr.xml is the one
;; maintained in the master branch of the Khronos OpenXR GitHub project.
;; types :
(defctype bool-32 :uint32)

(defctype flags-64 :uint64)

(defctype time :int64)

(defctype duration :int64)

(defctype version :uint64)

(defctype path atom)

(defctype system-id atom)

(defbitfield instance-create-flags)

(defbitfield session-create-flags)

(defbitfield swapchain-create-flags
  ;; content will be protected from cpu access
  (:protected-content #x00000001)
  ;; only one image will be acquired from this swapchain over its lifetime
  (:static-image #x00000002))

(defbitfield swapchain-usage-flags
  ;; specifies that the image can: be a color rendering target.
  (:color-attachment #x00000001)
  ;; specifies that the image can: be a depth/stencil rendering target.
  (:depth-stencil-attachment #x00000002)
  ;; specifies that the image can: be used as data/compute.
  (:unordered-access #x00000004)
  ;; specifies that the image can: be used as the source of a transfer command.
  (:transfer-src #x00000008)
  ;; specifies that the image can: be used as the destination of a transfer command.
  (:transfer-dst #x00000010)
  ;; specifies that the image can: be sampled by a shader.
  (:sampled #x00000020)
  ;; specifies that the image can: be reinterpreted as another image format.
  (:mutable-format #x00000040))

(defbitfield view-state-flags
  ;; indicates validity of all xrview orientations
  (:orientation-valid #x00000001)
  ;; indicates validity of all xrview positions
  (:position-valid #x00000002)
  ;; indicates whether all xrview orientations are actively tracked
  (:orientation-tracked #x00000004)
  ;; indicates whether all xrview positions are actively tracked
  (:position-tracked #x00000008))

(defbitfield composition-layer-flags
  ;; enables chromatic aberration correction when not done by default.
  (:correct-chromatic-aberration #x00000001)
  ;; enables the layer texture alpha channel.
  (:blend-texture-source-alpha #x00000002)
  ;; indicates the texture color channels have not been premultiplied by the texture alpha channel.
  (:unpremultiplied-alpha #x00000004))

(defbitfield space-location-flags
  ;; indicates validity of orientation member
  (:orientation-valid #x00000001)
  ;; indicates validity of position member
  (:position-valid #x00000002)
  ;; indicates whether pose member contains an actively tracked orientation
  (:orientation-tracked #x00000004)
  ;; indicates whether pose member contains an actively tracked position
  (:position-tracked #x00000008))

(defbitfield space-velocity-flags
  ;; indicates validity of linearvelocity member
  (:linear-valid #x00000001)
  ;; indicates validity of angularvelocity member
  (:angular-valid #x00000002))

(defbitfield input-source-localized-name-flags
  ;; asks for the part of the string which indicates the top level user path the source represents
  (:user-path #x00000001)
  ;; asks for the part of the string which represents the interaction profile of the source
  (:interaction-profile #x00000002)
  ;; asks for the part of the string which represents the component on the device which needs to be interacted with
  (:component #x00000004))

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
  ;; indicates the main session enabled xr_khr_extra_layer_info_depth
  (:enabled-composition-layer-info-depth-extx #x00000001))

(defbitfield overlay-session-create-flags-extx
  ;; indicates the runtime does not need to attempt to lock the overlay session displaytime to the main session displaytime
  (:relaxed-display-time-extx #x00000001))

(defctype instance xr-handle)
(defctype session xr-handle)
(defctype action-set xr-handle)
(defctype action xr-handle)
(defctype swapchain xr-handle)
(defctype space xr-handle)
(defctype debug-utils-messenger-ext xr-handle)
(defctype spatial-anchor-msft xr-handle)
(defctype hand-tracker-ext xr-handle)
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
  (:type-spatial-anchor-create-info-msft 10000039000)
  (:type-spatial-anchor-space-create-info-msft 10000039001)
  (:type-view-configuration-depth-range-ext 10000046000)
  (:type-graphics-binding-egl-mndx 10000048004)
  (:type-spatial-graph-node-space-create-info-msft 10000049000)
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
  (:type-view-configuration-view-fov-epic 10000059000))

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
  ;; the provided xrtime was zero, negative, or out of range.
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
  ;; xrsetandroidapplicationthreadkhr failed as thread id is invalid.
  (:error-android-thread-settings-id-invalid-khr -10000003000)
  ;; xrsetandroidapplicationthreadkhr failed setting the thread attributes/priority.
  (:error-android-thread-settings-failure-khr -10000003001)
  ;; spatial anchor could not be created at that location.
  (:error-create-spatial-anchor-failed-msft -10000039001)
  ;; the secondary view configuration was not enabled when creating the session.
  (:error-secondary-view-configuration-type-not-enabled-msft -10000053000))

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
  ;; xrhandtrackerext
  (:hand-tracker-ext 10000051000))

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
  (:unbounded-msft 10000038000))

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
  ;; performance settings hint used by the application to indicate that it enters a non-xr                  section (head-locked / static screen), during which power savings are to be prioritized
  (:power-savings-ext 0)
  ;; performance settings hint used by the application to indicate that it enters a low                  and stable complexity section, during which reducing power is more important than                  occasional late rendering frames
  (:sustained-low-ext 25)
  ;; performance settings hint used by the application to indicate that it enters                  a high or dynamic complexity section, during which the xr runtime strives for consistent                  xr compositing and frame rendering within a thermally sustainable range
  (:sustained-high-ext 50)
  ;; performance settings hint used by the application to indicate that the application enters                  a section with very high complexity, during which the xr runtime is allowed to step                  up beyond the thermally sustainable range
  (:boost-ext 75))

(defcenum perf-settings-notification-level-ext
  ;; notifies that the sub-domain has reached a level                  where no further actions other than currently applied are necessary
  (:level-normal-ext 0)
  ;; notifies that the sub-domain has reached an early warning level                  where the application should start proactive mitigation actions                  with the goal to return to the ename:xr_perf_notif_level_normal level
  (:level-warning-ext 25)
  ;; notifies that the sub-domain has reached a critical                  level with significant performance degradation.                  the application should take drastic mitigation action
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
  (layer-name :char)
  (spec-version version)
  (layer-version :uint32)
  (description :char))

(defcstruct extension-properties
  (type structure-type) ;; = type-extension-properties
  (next (:pointer (:pointer :void)))
  (extension-name :char)
  (extension-version :uint32))

(defcstruct application-info
  (application-name :char)
  (application-version :uint32)
  (engine-name :char)
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
  (runtime-name :char))

(defcstruct system-get-info
  (type structure-type) ;; = type-system-get-info
  (next (:pointer (:pointer :void)))
  (form-factor form-factor))

(defcstruct system-graphics-properties
  (max-swapchain-image-height :uint32)
  (max-swapchain-image-width :uint32)
  (max-layer-count :uint32))

(defcstruct system-tracking-properties
  (orientation-tracking bool-32)
  (position-tracking bool-32))


(defcstruct system-properties
  (type structure-type) ;; = type-system-properties
  (next (:pointer (:pointer :void)))
  (system-id system-id)
  (vendor-id :uint32)
  (system-name :char)
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
  (varying :uint8))

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
  (action-set-name :char)
  (localized-action-set-name :char)
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
  (action-name :char)
  (action-type action-type)
  (count-subaction-paths :uint32) ;; optional
  (subaction-paths (:pointer (:pointer path))) ;; count count-subaction-paths, optional
  (localized-action-name :char))

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
  (recommended-mutable-fov (:struct fov-f))
  (max-mutable-fov (:struct fov-f)))

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

(defcstruct graphics-binding-eglmndx
  (type structure-type) ;; = type-graphics-binding-egl-mndx
  (next (:pointer (:pointer :void)))
  (get-proc-address pfn-egl-get-proc-address-proc)
  (display egl-display)
  (config egl-config)
  (context egl-context))

(defcstruct spatial-graph-node-space-create-info-msft
  (type structure-type) ;; = type-spatial-graph-node-space-create-info-msft
  (next (:pointer (:pointer :void)))
  (node-type spatial-graph-node-type-msft)
  (node-id :uint8)
  (pose (:struct pose-f)))

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
  (:default-ext 0))

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
  (location-flags space-location-flags)
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

(defcstruct hand-mesh-index-buffer-msft
  (index-buffer-key :uint32) ;; optional
  (index-capacity-input :uint32)
  (index-count-output :uint32) ;; optional
  (indices (:pointer (:pointer :uint32))) ;; count index-capacity-input
)

(defcstruct hand-mesh-vertex-msft
  (position (:struct vector-3f))
  (normal (:struct vector-3f)))

(defcstruct hand-mesh-vertex-buffer-msft
  (vertex-update-time time) ;; optional
  (vertex-capacity-input :uint32)
  (vertex-count-output :uint32) ;; optional
  (vertices (:pointer (:pointer (:struct hand-mesh-vertex-msft)))) ;; count vertex-capacity-input
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

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-out-of-memory error-function-unsupported
;;          error-validation-failure)
(defcfun ("xrGetInstanceProcAddr" get-instance-proc-addr) result
  ;; optional = true
 (instance instance)
  ;; count = null-terminated
 (name (:pointer :string))
 (function (:pointer :pointer)))

;; success success
;;  errors (error-out-of-memory error-validation-failure error-runtime-failure
;;          error-size-insufficient)
(defcfun ("xrEnumerateApiLayerProperties" enumerate-api-layer-properties) result
  ;; optional = true
 (property-capacity-input :uint32)
 (property-count-output (:pointer :uint32))
  ;; count = property-capacity-input
  ;; optional = true
 (properties (:pointer (:struct api-layer-properties))))

;; success success
;;  errors (error-out-of-memory error-api-layer-not-present error-runtime-failure
;;          error-validation-failure error-size-insufficient)
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
;;  errors (error-out-of-memory error-limit-reached error-instance-lost
;;          error-runtime-failure error-initialization-failed
;;          error-api-version-unsupported error-api-layer-not-present
;;          error-extension-not-present error-validation-failure
;;          error-name-invalid)
(defcfun ("xrCreateInstance" create-instance) result
 (create-info (:pointer (:struct instance-create-info)))
 (instance (:pointer instance)))

;; success success
;;  errors (error-handle-invalid)
(defcfun ("xrDestroyInstance" destroy-instance) result
  ;; externsync = true_with_children
 (instance instance))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure)
(defcfun ("xrResultToString" result-to-string) result
 (instance instance)
 (value result)
 (buffer :char))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure)
(defcfun ("xrStructureTypeToString" structure-type-to-string) result
 (instance instance)
 (value structure-type)
 (buffer :char))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure)
(defcfun ("xrGetInstanceProperties" get-instance-properties) result
 (instance instance)
 (instance-properties (:pointer (:struct instance-properties))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-form-factor-unavailable error-form-factor-unsupported
;;          error-validation-failure)
(defcfun ("xrGetSystem" get-system) result
 (instance instance)
 (get-info (:pointer (:struct system-get-info)))
 (system-id (:pointer system-id)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-out-of-memory error-system-invalid error-validation-failure)
(defcfun ("xrGetSystemProperties" get-system-properties) result
 (instance instance)
 (system-id system-id)
 (properties (:pointer (:struct system-properties))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-out-of-memory error-limit-reached error-initialization-failed
;;          error-system-invalid error-graphics-device-invalid
;;          error-validation-failure)
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
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-size-insufficient error-validation-failure)
(defcfun ("xrEnumerateSwapchainFormats" enumerate-swapchain-formats) result
 (session session)
  ;; optional = true
 (format-capacity-input :uint32)
 (format-count-output (:pointer :uint32))
  ;; count = format-capacity-input
  ;; optional = true
 (formats (:pointer :int64)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-limit-reached error-handle-invalid error-out-of-memory
;;          error-swapchain-format-unsupported error-feature-unsupported
;;          error-validation-failure)
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
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-size-insufficient error-handle-invalid error-validation-failure)
(defcfun ("xrEnumerateSwapchainImages" enumerate-swapchain-images) result
 (swapchain swapchain)
  ;; optional = true
 (image-capacity-input :uint32)
 (image-count-output (:pointer :uint32))
  ;; count = image-capacity-input
  ;; optional = true
 (images (:pointer (:struct swapchain-image-base-header))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-call-order-invalid)
(defcfun ("xrAcquireSwapchainImage" acquire-swapchain-image) result
 (swapchain swapchain)
  ;; optional = true
 (acquire-info (:pointer (:struct swapchain-image-acquire-info)))
 (index (:pointer :uint32)))

;; success success,xr-timeout-expired,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-call-order-invalid)
(defcfun ("xrWaitSwapchainImage" wait-swapchain-image) result
 (swapchain swapchain)
 (wait-info (:pointer (:struct swapchain-image-wait-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-call-order-invalid)
(defcfun ("xrReleaseSwapchainImage" release-swapchain-image) result
 (swapchain swapchain)
  ;; optional = true
 (release-info (:pointer (:struct swapchain-image-release-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-session-not-ready
;;          error-session-running error-view-configuration-type-unsupported)
(defcfun ("xrBeginSession" begin-session) result
 (session session)
 (begin-info (:pointer (:struct session-begin-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-session-not-stopping
;;          error-session-not-running error-validation-failure)
(defcfun ("xrEndSession" end-session) result
 (session session))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-session-not-running
;;          error-validation-failure)
(defcfun ("xrRequestExitSession" request-exit-session) result
 (session session))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-size-insufficient error-validation-failure)
(defcfun ("xrEnumerateReferenceSpaces" enumerate-reference-spaces) result
 (session session)
  ;; optional = true
 (space-capacity-input :uint32)
 (space-count-output (:pointer :uint32))
  ;; count = space-capacity-input
  ;; optional = true
 (spaces (:pointer reference-space-type)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-limit-reached error-out-of-memory error-handle-invalid
;;          error-reference-space-unsupported error-pose-invalid
;;          error-validation-failure)
(defcfun ("xrCreateReferenceSpace" create-reference-space) result
 (session session)
 (create-info (:pointer (:struct reference-space-create-info)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-out-of-memory error-handle-invalid error-action-type-mismatch
;;          error-limit-reached error-pose-invalid error-validation-failure
;;          error-path-unsupported error-path-invalid)
(defcfun ("xrCreateActionSpace" create-action-space) result
 (session session)
 (create-info (:pointer (:struct action-space-create-info)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-time-invalid)
(defcfun ("xrLocateSpace" locate-space) result
 (space space)
 (base-space space)
 (time time)
 (location (:pointer (:struct space-location))))

;; success success
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-system-invalid error-validation-failure error-size-insufficient)
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
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-system-invalid error-validation-failure
;;          error-view-configuration-type-unsupported error-size-insufficient)
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
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-system-invalid error-validation-failure
;;          error-view-configuration-type-unsupported)
(defcfun ("xrGetViewConfigurationProperties" get-view-configuration-properties) result
 (instance instance)
 (system-id system-id)
 (view-configuration-type view-configuration-type)
 (configuration-properties (:pointer (:struct view-configuration-properties))))

;; success success
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-system-invalid error-validation-failure
;;          error-view-configuration-type-unsupported error-size-insufficient)
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
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-call-order-invalid
;;          error-session-not-running error-validation-failure)
(defcfun ("xrBeginFrame" begin-frame) result
 (session session)
  ;; optional = true
 (frame-begin-info (:pointer (:struct frame-begin-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-size-insufficient error-validation-failure
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
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-call-order-invalid error-layer-invalid
;;          error-swapchain-rect-invalid error-environment-blend-mode-unsupported
;;          error-session-not-running error-layer-limit-exceeded
;;          error-validation-failure error-time-invalid error-pose-invalid)
(defcfun ("xrEndFrame" end-frame) result
 (session session)
 (frame-end-info (:pointer (:struct frame-end-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-session-not-running
;;          error-validation-failure)
(defcfun ("xrWaitFrame" wait-frame) result
 (session session)
  ;; optional = true
 (frame-wait-info (:pointer (:struct frame-wait-info)))
 (frame-state (:pointer (:struct frame-state)))
  ;; implicit external sync params:
  ;; the pname:session parameter by any other flink:xrWaitFrame call
)

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure
;;          error-actionset-not-attached error-action-type-mismatch
;;          error-path-invalid error-path-unsupported)
(defcfun ("xrApplyHapticFeedback" apply-haptic-feedback) result
 (session session)
 (haptic-action-info (:pointer (:struct haptic-action-info)))
 (haptic-feedback (:pointer (:struct haptic-base-header))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure
;;          error-actionset-not-attached error-action-type-mismatch
;;          error-path-invalid error-path-unsupported)
(defcfun ("xrStopHapticFeedback" stop-haptic-feedback) result
 (session session)
 (haptic-action-info (:pointer (:struct haptic-action-info))))

;; success success,xr-event-unavailable
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-validation-failure)
(defcfun ("xrPollEvent" poll-event) result
 (instance instance)
 (event-data (:pointer (:struct event-data-buffer))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-path-format-invalid error-path-count-exceeded
;;          error-validation-failure)
(defcfun ("xrStringToPath" string-to-path) result
 (instance instance)
  ;; count = null-terminated
 (path-string (:pointer :string))
 (path (:pointer path)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-path-invalid error-size-insufficient error-validation-failure)
(defcfun ("xrPathToString" path-to-string) result
 (instance instance)
 (path path)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = buffer-capacity-input
  ;; optional = true
 (buffer (:pointer :char)))

;; success success,xr-space-bounds-unavailable,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure
;;          error-function-unsupported error-reference-space-unsupported)
(defcfun ("xrGetReferenceSpaceBoundsRect" get-reference-space-bounds-rect) result
 (session session)
 (reference-space-type reference-space-type)
 (bounds (:pointer (:struct extent-2d-f))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-android-thread-settings-id-invalid-khr
;;          error-android-thread-settings-failure-khr error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrSetAndroidApplicationThreadKHR" set-android-application-thread-khr) result
 (session session)
 (thread-type android-thread-type-khr)
 (thread-id :uint32))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-limit-reached error-handle-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrCreateSwapchainAndroidSurfaceKHR" create-swapchain-android-surface-khr) result
 (session session)
 (info (:pointer (:struct swapchain-create-info)))
 (swapchain (:pointer swapchain))
 (surface (:pointer j-object)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-action-type-mismatch error-validation-failure error-path-invalid
;;          error-path-unsupported)
(defcfun ("xrGetActionStateBoolean" get-action-state-boolean) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-boolean))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-action-type-mismatch error-validation-failure error-path-invalid
;;          error-path-unsupported)
(defcfun ("xrGetActionStateFloat" get-action-state-float) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-float))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-action-type-mismatch error-validation-failure error-path-invalid
;;          error-path-unsupported)
(defcfun ("xrGetActionStateVector2f" get-action-state-vector-2f) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-vector-2f))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-action-type-mismatch error-validation-failure error-path-invalid
;;          error-path-unsupported)
(defcfun ("xrGetActionStatePose" get-action-state-pose) result
 (session session)
 (get-info (:pointer (:struct action-state-get-info)))
 (state (:pointer (:struct action-state-pose))))

;; success success
;;  errors (error-instance-lost error-runtime-failure error-limit-reached
;;          error-handle-invalid error-out-of-memory error-validation-failure
;;          error-name-duplicated error-localized-name-duplicated
;;          error-name-invalid error-localized-name-invalid
;;          error-path-format-invalid)
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
;;  errors (error-instance-lost error-runtime-failure
;;          error-actionsets-already-attached error-limit-reached
;;          error-handle-invalid error-out-of-memory error-path-invalid
;;          error-validation-failure error-name-duplicated
;;          error-localized-name-duplicated error-name-invalid
;;          error-localized-name-invalid error-path-format-invalid
;;          error-path-unsupported)
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
;;  errors (error-instance-lost error-runtime-failure
;;          error-actionsets-already-attached error-handle-invalid
;;          error-validation-failure error-path-unsupported error-path-invalid)
(defcfun ("xrSuggestInteractionProfileBindings" suggest-interaction-profile-bindings) result
 (instance instance)
 (suggested-bindings (:pointer (:struct interaction-profile-suggested-binding))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure
;;          error-actionsets-already-attached)
(defcfun ("xrAttachSessionActionSets" attach-session-action-sets) result
 (session session)
 (attach-info (:pointer (:struct session-action-sets-attach-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-validation-failure error-path-unsupported error-path-invalid)
(defcfun ("xrGetCurrentInteractionProfile" get-current-interaction-profile) result
 (session session)
 (top-level-user-path path)
 (interaction-profile (:pointer (:struct interaction-profile-state))))

;; success success,xr-session-loss-pending,xr-session-not-focused
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-validation-failure error-path-invalid error-path-unsupported)
(defcfun ("xrSyncActions" sync-actions) result
 (session session)
 (sync-info (:pointer (:struct actions-sync-info))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-actionset-not-attached
;;          error-size-insufficient error-validation-failure error-path-invalid)
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
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-size-insufficient
;;          error-path-invalid error-path-unsupported error-actionset-not-attached)
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
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-system-invalid error-validation-failure error-size-insufficient
;;          error-function-unsupported)
(defcfun ("xrGetVulkanInstanceExtensionsKHR" get-vulkan-instance-extensions-khr) result
 (instance instance)
 (system-id system-id)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = |buffer-capacity-input,null-terminated|
  ;; optional = true
 (buffer (:pointer :string)))

;; success success
;;  errors (error-instance-lost error-runtime-failure error-handle-invalid
;;          error-system-invalid error-validation-failure error-size-insufficient
;;          error-function-unsupported)
(defcfun ("xrGetVulkanDeviceExtensionsKHR" get-vulkan-device-extensions-khr) result
 (instance instance)
 (system-id system-id)
  ;; optional = true
 (buffer-capacity-input :uint32)
 (buffer-count-output (:pointer :uint32))
  ;; count = |buffer-capacity-input,null-terminated|
  ;; optional = true
 (buffer (:pointer :string)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-system-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrGetVulkanGraphicsDeviceKHR" get-vulkan-graphics-device-khr) result
 (instance instance)
 (system-id system-id)
 (vk-instance vk-image)
 (vk-physical-device (:pointer vk-physical-device)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-system-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrGetOpenGLGraphicsRequirementsKHR" get-opengl-graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-opengl-khr))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-system-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrGetOpenGLESGraphicsRequirementsKHR" get-opengl-esgraphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-opengl-es-khr))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-system-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrGetVulkanGraphicsRequirementsKHR" get-vulkan-graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-vulkan-khr))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-system-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrGetD3D11GraphicsRequirementsKHR" get-d3d11graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-d3d11-khr))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-system-invalid error-validation-failure
;;          error-function-unsupported)
(defcfun ("xrGetD3D12GraphicsRequirementsKHR" get-d3d12graphics-requirements-khr) result
 (instance instance)
 (system-id system-id)
 (graphics-requirements (:pointer (:struct graphics-requirements-d3d12-khr))))

;; success success,xr-session-loss-pending
;;  errors (error-session-lost error-handle-invalid error-validation-failure
;;          error-runtime-failure error-function-unsupported)
(defcfun ("xrPerfSettingsSetPerformanceLevelEXT" perf-settings-set-performance-level-ext) result
 (session session)
 (domain perf-settings-domain-ext)
 (level perf-settings-level-ext))

;; success success,xr-session-loss-pending
;;  errors (error-session-lost error-handle-invalid error-validation-failure
;;          error-runtime-failure error-function-unsupported)
(defcfun ("xrThermalGetTemperatureTrendEXT" thermal-get-temperature-trend-ext) result
 (session session)
 (domain perf-settings-domain-ext)
 (notification-level (:pointer perf-settings-notification-level-ext))
 (temp-headroom (:pointer :float))
 (temp-slope (:pointer :float)))

;; success success
;;  errors (error-out-of-memory error-validation-failure error-runtime-failure
;;          error-handle-invalid error-instance-lost error-function-unsupported)
(defcfun ("xrSetDebugUtilsObjectNameEXT" set-debug-utils-object-name-ext) result
 (instance instance)
  ;; externsync = nameinfo.objecthandle
 (name-info (:pointer (:struct debug-utils-object-name-info-ext))))

;; success success
;;  errors (error-out-of-memory error-validation-failure error-runtime-failure
;;          error-limit-reached error-handle-invalid error-instance-lost
;;          error-function-unsupported)
(defcfun ("xrCreateDebugUtilsMessengerEXT" create-debug-utils-messenger-ext) result
  ;; externsync = true_with_children
 (instance instance)
 (create-info (:pointer (:struct debug-utils-messenger-create-info-ext)))
 (messenger (:pointer debug-utils-messenger-ext)))

;; success success
;;  errors (error-handle-invalid error-function-unsupported)
(defcfun ("xrDestroyDebugUtilsMessengerEXT" destroy-debug-utils-messenger-ext) result
  ;; externsync = true
 (messenger debug-utils-messenger-ext)
  ;; implicit external sync params:
  ;; the slink:XrInstance used to create pname:messenger, and all of its child handles
)

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure error-function-unsupported)
(defcfun ("xrSubmitDebugUtilsMessageEXT" submit-debug-utils-message-ext) result
 (instance instance)
 (message-severity debug-utils-message-severity-flags-ext)
 (message-types debug-utils-message-type-flags-ext)
 (callback-data (:pointer (:struct debug-utils-messenger-callback-data-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-session-lost error-handle-invalid error-validation-failure
;;          error-runtime-failure error-function-unsupported)
(defcfun ("xrSessionBeginDebugUtilsLabelRegionEXT" session-begin-debug-utils-label-region-ext) result
 (session session)
 (label-info (:pointer (:struct debug-utils-label-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-session-lost error-handle-invalid error-validation-failure
;;          error-runtime-failure error-function-unsupported)
(defcfun ("xrSessionEndDebugUtilsLabelRegionEXT" session-end-debug-utils-label-region-ext) result
 (session session))

;; success success,xr-session-loss-pending
;;  errors (error-session-lost error-handle-invalid error-validation-failure
;;          error-runtime-failure error-function-unsupported)
(defcfun ("xrSessionInsertDebugUtilsLabelEXT" session-insert-debug-utils-label-ext) result
 (session session)
 (label-info (:pointer (:struct debug-utils-label-ext))))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure error-time-invalid error-function-unsupported)
(defcfun ("xrConvertTimeToWin32PerformanceCounterKHR" convert-time-to-win-32performance-counter-khr) result
 (instance instance)
 (time time)
 (performance-counter (:pointer large-integer)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure error-time-invalid error-function-unsupported)
(defcfun ("xrConvertWin32PerformanceCounterToTimeKHR" convert-win-32performance-counter-to-time-khr) result
 (instance instance)
 (performance-counter (:pointer large-integer))
 (time (:pointer time)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure error-time-invalid error-function-unsupported)
(defcfun ("xrConvertTimeToTimespecTimeKHR" convert-time-to-timespec-time-khr) result
 (instance instance)
 (time time)
 (timespec-time (:pointer timespec)))

;; success success
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure error-time-invalid error-function-unsupported)
(defcfun ("xrConvertTimespecTimeToTimeKHR" convert-timespec-time-to-time-khr) result
 (instance instance)
 (timespec-time (:pointer timespec))
 (time (:pointer time)))

;; success success,xr-session-loss-pending
;;  errors (error-handle-invalid error-instance-lost error-runtime-failure
;;          error-validation-failure error-view-configuration-type-unsupported
;;          error-size-insufficient error-session-lost error-function-unsupported)
(defcfun ("xrGetVisibilityMaskKHR" get-visibility-mask-khr) result
 (session session)
 (view-configuration-type view-configuration-type)
 (view-index :uint32)
 (visibility-mask-type visibility-mask-type-khr)
 (visibility-mask (:pointer (:struct visibility-mask-khr))))

;; success success
;;  errors (error-instance-lost error-out-of-memory error-function-unsupported
;;          error-validation-failure error-handle-invalid error-pose-invalid
;;          error-create-spatial-anchor-failed-msft error-time-invalid
;;          error-session-lost session-loss-pending)
(defcfun ("xrCreateSpatialAnchorMSFT" create-spatial-anchor-msft) result
 (session session)
 (create-info (:pointer (:struct spatial-anchor-create-info-msft)))
 (anchor (:pointer spatial-anchor-msft)))

;; success success
;;  errors (error-instance-lost error-out-of-memory error-function-unsupported
;;          error-validation-failure error-handle-invalid error-pose-invalid
;;          error-session-lost session-loss-pending)
(defcfun ("xrCreateSpatialAnchorSpaceMSFT" create-spatial-anchor-space-msft) result
 (session session)
 (create-info (:pointer (:struct spatial-anchor-space-create-info-msft)))
 (space (:pointer space)))

;; success success
;;  errors (error-handle-invalid error-function-unsupported)
(defcfun ("xrDestroySpatialAnchorMSFT" destroy-spatial-anchor-msft) result
 (anchor spatial-anchor-msft))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-session-lost session-loss-pending
;;          error-path-invalid error-path-unsupported)
(defcfun ("xrSetInputDeviceActiveEXT" set-input-device-active-ext) result
 (session session)
 (interaction-profile path)
 (top-level-path path)
 (is-active bool-32))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-session-lost session-loss-pending
;;          error-path-invalid error-path-unsupported)
(defcfun ("xrSetInputDeviceStateBoolEXT" set-input-device-state-bool-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (state bool-32))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-session-lost session-loss-pending
;;          error-path-invalid error-path-unsupported)
(defcfun ("xrSetInputDeviceStateFloatEXT" set-input-device-state-float-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (state :float))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-session-lost session-loss-pending
;;          error-path-invalid error-path-unsupported)
#++
(defcfun ("xrSetInputDeviceStateVector2fEXT" set-input-device-state-vector-2f-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (state (:struct vector-2f)))

;; success success
;;  errors (error-function-unsupported error-validation-failure
;;          error-handle-invalid error-session-lost session-loss-pending
;;          error-path-invalid error-path-unsupported error-pose-invalid)
#++
(defcfun ("xrSetInputDeviceLocationEXT" set-input-device-location-ext) result
 (session session)
 (top-level-path path)
 (input-source-path path)
 (space space)
 (pose (:struct pose-f)))

;; success success
;;  errors (error-instance-lost error-out-of-memory error-function-unsupported
;;          error-validation-failure error-handle-invalid error-session-lost
;;          session-loss-pending error-pose-invalid)
(defcfun ("xrCreateSpatialGraphNodeSpaceMSFT" create-spatial-graph-node-space-msft) result
 (session session)
 (create-info (:pointer (:struct spatial-graph-node-space-create-info-msft)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure
;;          error-function-unsupported error-feature-unsupported)
(defcfun ("xrCreateHandTrackerEXT" create-hand-tracker-ext) result
 (session session)
 (create-info (:pointer (:struct hand-tracker-create-info-ext)))
 (hand-tracker (:pointer hand-tracker-ext)))

;; success success
;;  errors (error-handle-invalid error-function-unsupported)
(defcfun ("xrDestroyHandTrackerEXT" destroy-hand-tracker-ext) result
  ;; externsync = true_with_children
 (hand-tracker hand-tracker-ext))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-time-invalid
;;          error-function-unsupported)
(defcfun ("xrLocateHandJointsEXT" locate-hand-joints-ext) result
 (hand-tracker hand-tracker-ext)
 (locate-info (:pointer (:struct hand-joints-locate-info-ext)))
 (locations (:pointer (:struct hand-joint-locations-ext))))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-pose-invalid error-validation-failure
;;          error-function-unsupported error-feature-unsupported)
(defcfun ("xrCreateHandMeshSpaceMSFT" create-hand-mesh-space-msft) result
 (hand-tracker hand-tracker-ext)
 (create-info (:pointer (:struct hand-mesh-space-create-info-msft)))
 (space (:pointer space)))

;; success success,xr-session-loss-pending
;;  errors (error-instance-lost error-session-lost error-runtime-failure
;;          error-handle-invalid error-validation-failure error-time-invalid
;;          error-function-unsupported error-size-insufficient
;;          error-feature-unsupported)
(defcfun ("xrUpdateHandMeshMSFT" update-hand-mesh-msft) result
 (hand-tracker hand-tracker-ext)
 (update-info (:pointer (:struct hand-mesh-update-info-msft)))
 (hand-mesh (:pointer (:struct hand-mesh-msft))))

