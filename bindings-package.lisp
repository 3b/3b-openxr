(defpackage #:3b-openxr-bindings
  (:use :cl #:cffi)
  (:shadow #:space #:time #:atom)
  (:export #:acquire-swapchain-image
           #:action-create-info
           #:action-set-create-info
           #:action-space-create-info
           #:action-state-boolean
           #:action-state-float
           #:action-state-get-info
           #:action-state-pose
           #:action-state-vector-2f
           #:action-suggested-binding
           #:actions-sync-info
           #:active-action-set
           #:api-layer-properties
           #:application-info
           #:apply-haptic-feedback
           #:attach-session-action-sets
           #:base-in-structure
           #:base-out-structure
           #:begin-frame
           #:begin-session
           #:bound-sources-for-action-enumerate-info
           #:color-4f
           #:composition-layer-base-header
           #:composition-layer-cube-khr
           #:composition-layer-cylinder-khr
           #:composition-layer-depth-info-khr
           #:composition-layer-equirect-khr
           #:composition-layer-projection
           #:composition-layer-projection-view
           #:composition-layer-quad
           #:convert-time-to-timespec-time-khr
           #:convert-time-to-win-32performance-counter-khr
           #:convert-timespec-time-to-time-khr
           #:convert-win-32performance-counter-to-time-khr
           #:create-action
           #:create-action-set
           #:create-action-space
           #:create-debug-utils-messenger-ext
           #:create-hand-mesh-space-msft
           #:create-hand-tracker-ext
           #:create-instance
           #:create-reference-space
           #:create-session
           #:create-spatial-anchor-msft
           #:create-spatial-anchor-space-msft
           #:create-spatial-graph-node-space-msft
           #:create-swapchain
           #:create-swapchain-android-surface-khr
           #:debug-utils-label-ext
           #:debug-utils-messenger-callback-data-ext
           #:debug-utils-messenger-create-info-ext
           #:debug-utils-object-name-info-ext
           #:destroy-action
           #:destroy-action-set
           #:destroy-debug-utils-messenger-ext
           #:destroy-hand-tracker-ext
           #:destroy-instance
           #:destroy-session
           #:destroy-space
           #:destroy-spatial-anchor-msft
           #:destroy-swapchain
           #:end-frame
           #:end-session
           #:enumerate-api-layer-properties
           #:enumerate-bound-sources-for-action
           #:enumerate-environment-blend-modes
           #:enumerate-instance-extension-properties
           #:enumerate-reference-spaces
           #:enumerate-swapchain-formats
           #:enumerate-swapchain-images
           #:enumerate-view-configuration-views
           #:enumerate-view-configurations
           #:event-data-base-header
           #:event-data-buffer
           #:event-data-events-lost
           #:event-data-instance-loss-pending
           #:event-data-interaction-profile-changed
           #:event-data-main-session-visibility-changed-extx
           #:event-data-perf-settings-ext
           #:event-data-reference-space-change-pending
           #:event-data-session-state-changed
           #:event-data-visibility-mask-changed-khr
           #:extension-properties
           #:extent-2d-f
           #:extent-2d-i
           #:eye-gaze-sample-time-ext
           #:fov-f
           #:frame-begin-info
           #:frame-end-info
           #:frame-state
           #:frame-wait-info
           #:get-action-state-boolean
           #:get-action-state-float
           #:get-action-state-pose
           #:get-action-state-vector-2f
           #:get-current-interaction-profile
           #:get-d3d11graphics-requirements-khr
           #:get-d3d12graphics-requirements-khr
           #:get-input-source-localized-name
           #:get-instance-proc-addr
           #:get-instance-properties
           #:get-opengl-esgraphics-requirements-khr
           #:get-opengl-graphics-requirements-khr
           #:get-reference-space-bounds-rect
           #:get-system
           #:get-system-properties
           #:get-view-configuration-properties
           #:get-visibility-mask-khr
           #:get-vulkan-device-extensions-khr
           #:get-vulkan-graphics-device-khr
           #:get-vulkan-graphics-requirements-khr
           #:get-vulkan-instance-extensions-khr
           #:graphics-binding-d3d11-khr
           #:graphics-binding-d3d12-khr
           #:graphics-binding-eglmndx
           #:graphics-binding-opengl-esandroid-khr
           #:graphics-binding-opengl-wayland-khr
           #:graphics-binding-opengl-win-32-khr
           #:graphics-binding-opengl-xcb-khr
           #:graphics-binding-opengl-xlib-khr
           #:graphics-binding-vulkan-khr
           #:graphics-requirements-d3d11-khr
           #:graphics-requirements-d3d12-khr
           #:graphics-requirements-opengl-es-khr
           #:graphics-requirements-opengl-khr
           #:graphics-requirements-vulkan-khr
           #:hand-joint-location-ext
           #:hand-joint-locations-ext
           #:hand-joint-velocities-ext
           #:hand-joint-velocity-ext
           #:hand-joints-locate-info-ext
           #:hand-mesh-index-buffer-msft
           #:hand-mesh-msft
           #:hand-mesh-space-create-info-msft
           #:hand-mesh-update-info-msft
           #:hand-mesh-vertex-buffer-msft
           #:hand-mesh-vertex-msft
           #:hand-pose-type-info-msft
           #:hand-tracker-create-info-ext
           #:haptic-action-info
           #:haptic-base-header
           #:haptic-vibration
           #:input-source-localized-name-get-info
           #:instance-create-info
           #:instance-create-info-android-khr
           #:instance-properties
           #:interaction-profile-state
           #:interaction-profile-suggested-binding
           #:locate-hand-joints-ext
           #:locate-space
           #:locate-views
           #:offset-2d-f
           #:offset-2d-i
           #:path-to-string
           #:perf-settings-set-performance-level-ext
           #:poll-event
           #:pose-f
           #:quaternion-f
           #:rect-2d-f
           #:rect-2d-i
           #:reference-space-create-info
           #:release-swapchain-image
           #:request-exit-session
           #:result-to-string
           #:secondary-view-configuration-frame-end-info-msft
           #:secondary-view-configuration-frame-state-msft
           #:secondary-view-configuration-layer-info-msft
           #:secondary-view-configuration-session-begin-info-msft
           #:secondary-view-configuration-state-msft
           #:secondary-view-configuration-swapchain-create-info-msft
           #:session-action-sets-attach-info
           #:session-begin-debug-utils-label-region-ext
           #:session-begin-info
           #:session-create-info
           #:session-create-info-overlay-extx
           #:session-end-debug-utils-label-region-ext
           #:session-insert-debug-utils-label-ext
           #:set-android-application-thread-khr
           #:set-debug-utils-object-name-ext
           #:set-input-device-active-ext
           #:set-input-device-location-ext
           #:set-input-device-state-bool-ext
           #:set-input-device-state-float-ext
           #:set-input-device-state-vector-2f-ext
           #:space-location
           #:space-velocity
           #:spatial-anchor-create-info-msft
           #:spatial-anchor-space-create-info-msft
           #:spatial-graph-node-space-create-info-msft
           #:stop-haptic-feedback
           #:string-to-path
           #:structure-type-to-string
           #:submit-debug-utils-message-ext
           #:suggest-interaction-profile-bindings
           #:swapchain-create-info
           #:swapchain-image-acquire-info
           #:swapchain-image-base-header
           #:swapchain-image-d3d11-khr
           #:swapchain-image-d3d12-khr
           #:swapchain-image-opengl-es-khr
           #:swapchain-image-opengl-khr
           #:swapchain-image-release-info
           #:swapchain-image-vulkan-khr
           #:swapchain-image-wait-info
           #:swapchain-sub-image
           #:sync-actions
           #:system-eye-gaze-interaction-properties-ext
           #:system-get-info
           #:system-graphics-properties
           #:system-hand-tracking-mesh-properties-msft
           #:system-hand-tracking-properties-ext
           #:system-properties
           #:system-tracking-properties
           #:thermal-get-temperature-trend-ext
           #:update-hand-mesh-msft
           #:vector-2f
           #:vector-3f
           #:vector-4f
           #:view
           #:view-configuration-depth-range-ext
           #:view-configuration-properties
           #:view-configuration-view
           #:view-configuration-view-fov-epic
           #:view-locate-info
           #:view-state
           #:visibility-mask-khr
           #:vulkan-swapchain-format-list-create-info-khr
           #:wait-frame
           #:wait-swapchain-image))
