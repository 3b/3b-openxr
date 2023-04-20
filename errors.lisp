(in-package #:3b-openxr-wrappers)

(defvar *error-conditions* (make-hash-table))

(define-condition error-validation-failure (simple-error)
  ())
(setf (gethash -1 *error-conditions*) 'error-validation-failure)

(define-condition error-runtime-failure (simple-error)
  ())
(setf (gethash -2 *error-conditions*) 'error-runtime-failure)

(define-condition error-out-of-memory (simple-error)
  ())
(setf (gethash -3 *error-conditions*) 'error-out-of-memory)

(define-condition error-api-version-unsupported (simple-error)
  ())
(setf (gethash -4 *error-conditions*) 'error-api-version-unsupported)

(define-condition error-initialization-failed (simple-error)
  ())
(setf (gethash -6 *error-conditions*) 'error-initialization-failed)

(define-condition error-function-unsupported (simple-error)
  ())
(setf (gethash -7 *error-conditions*) 'error-function-unsupported)

(define-condition error-feature-unsupported (simple-error)
  ())
(setf (gethash -8 *error-conditions*) 'error-feature-unsupported)

(define-condition error-extension-not-present (simple-error)
  ())
(setf (gethash -9 *error-conditions*) 'error-extension-not-present)

(define-condition error-limit-reached (simple-error)
  ())
(setf (gethash -10 *error-conditions*) 'error-limit-reached)

(define-condition error-size-insufficient (simple-error)
  ())
(setf (gethash -11 *error-conditions*) 'error-size-insufficient)

(define-condition error-handle-invalid (simple-error)
  ())
(setf (gethash -12 *error-conditions*) 'error-handle-invalid)

(define-condition error-instance-lost (simple-error)
  ())
(setf (gethash -13 *error-conditions*) 'error-instance-lost)

(define-condition error-session-running (simple-error)
  ())
(setf (gethash -14 *error-conditions*) 'error-session-running)

(define-condition error-session-not-running (simple-error)
  ())
(setf (gethash -16 *error-conditions*) 'error-session-not-running)

(define-condition error-session-lost (simple-error)
  ())
(setf (gethash -17 *error-conditions*) 'error-session-lost)

(define-condition error-system-invalid (simple-error)
  ())
(setf (gethash -18 *error-conditions*) 'error-system-invalid)

(define-condition error-path-invalid (simple-error)
  ())
(setf (gethash -19 *error-conditions*) 'error-path-invalid)

(define-condition error-path-count-exceeded (simple-error)
  ())
(setf (gethash -20 *error-conditions*) 'error-path-count-exceeded)

(define-condition error-path-format-invalid (simple-error)
  ())
(setf (gethash -21 *error-conditions*) 'error-path-format-invalid)

(define-condition error-path-unsupported (simple-error)
  ())
(setf (gethash -22 *error-conditions*) 'error-path-unsupported)

(define-condition error-layer-invalid (simple-error)
  ())
(setf (gethash -23 *error-conditions*) 'error-layer-invalid)

(define-condition error-layer-limit-exceeded (simple-error)
  ())
(setf (gethash -24 *error-conditions*) 'error-layer-limit-exceeded)

(define-condition error-swapchain-rect-invalid (simple-error)
  ())
(setf (gethash -25 *error-conditions*) 'error-swapchain-rect-invalid)

(define-condition error-swapchain-format-unsupported (simple-error)
  ())
(setf (gethash -26 *error-conditions*) 'error-swapchain-format-unsupported)

(define-condition error-action-type-mismatch (simple-error)
  ())
(setf (gethash -27 *error-conditions*) 'error-action-type-mismatch)

(define-condition error-session-not-ready (simple-error)
  ())
(setf (gethash -28 *error-conditions*) 'error-session-not-ready)

(define-condition error-session-not-stopping (simple-error)
  ())
(setf (gethash -29 *error-conditions*) 'error-session-not-stopping)

(define-condition error-time-invalid (simple-error)
  ())
(setf (gethash -30 *error-conditions*) 'error-time-invalid)

(define-condition error-reference-space-unsupported (simple-error)
  ())
(setf (gethash -31 *error-conditions*) 'error-reference-space-unsupported)

(define-condition error-file-access-error (simple-error)
  ())
(setf (gethash -32 *error-conditions*) 'error-file-access-error)

(define-condition error-file-contents-invalid (simple-error)
  ())
(setf (gethash -33 *error-conditions*) 'error-file-contents-invalid)

(define-condition error-form-factor-unsupported (simple-error)
  ())
(setf (gethash -34 *error-conditions*) 'error-form-factor-unsupported)

(define-condition error-form-factor-unavailable (simple-error)
  ())
(setf (gethash -35 *error-conditions*) 'error-form-factor-unavailable)

(define-condition error-api-layer-not-present (simple-error)
  ())
(setf (gethash -36 *error-conditions*) 'error-api-layer-not-present)

(define-condition error-call-order-invalid (simple-error)
  ())
(setf (gethash -37 *error-conditions*) 'error-call-order-invalid)

(define-condition error-graphics-device-invalid (simple-error)
  ())
(setf (gethash -38 *error-conditions*) 'error-graphics-device-invalid)

(define-condition error-pose-invalid (simple-error)
  ())
(setf (gethash -39 *error-conditions*) 'error-pose-invalid)

(define-condition error-index-out-of-range (simple-error)
  ())
(setf (gethash -40 *error-conditions*) 'error-index-out-of-range)

(define-condition error-view-configuration-type-unsupported (simple-error)
  ())
(setf (gethash -41 *error-conditions*) 'error-view-configuration-type-unsupported)

(define-condition error-environment-blend-mode-unsupported (simple-error)
  ())
(setf (gethash -42 *error-conditions*) 'error-environment-blend-mode-unsupported)

(define-condition error-name-duplicated (simple-error)
  ())
(setf (gethash -44 *error-conditions*) 'error-name-duplicated)

(define-condition error-name-invalid (simple-error)
  ())
(setf (gethash -45 *error-conditions*) 'error-name-invalid)

(define-condition error-actionset-not-attached (simple-error)
  ())
(setf (gethash -46 *error-conditions*) 'error-actionset-not-attached)

(define-condition error-actionsets-already-attached (simple-error)
  ())
(setf (gethash -47 *error-conditions*) 'error-actionsets-already-attached)

(define-condition error-localized-name-duplicated (simple-error)
  ())
(setf (gethash -48 *error-conditions*) 'error-localized-name-duplicated)

(define-condition error-localized-name-invalid (simple-error)
  ())
(setf (gethash -49 *error-conditions*) 'error-localized-name-invalid)

(define-condition error-graphics-requirements-call-missing (simple-error)
  ())
(setf (gethash -50 *error-conditions*) 'error-graphics-requirements-call-missing)

(define-condition error-runtime-unavailable (simple-error)
  ())
(setf (gethash -51 *error-conditions*) 'error-runtime-unavailable)

(define-condition error-android-thread-settings-id-invalid-khr (simple-error)
  ())
(setf (gethash -1000003000 *error-conditions*) 'error-android-thread-settings-id-invalid-khr)

(define-condition error-android-thread-settings-failure-khr (simple-error)
  ())
(setf (gethash -1000003001 *error-conditions*) 'error-android-thread-settings-failure-khr)

(define-condition error-create-spatial-anchor-failed-msft (simple-error)
  ())
(setf (gethash -1000039001 *error-conditions*) 'error-create-spatial-anchor-failed-msft)

(define-condition error-secondary-view-configuration-type-not-enabled-msft (simple-error)
  ())
(setf (gethash -1000053000 *error-conditions*) 'error-secondary-view-configuration-type-not-enabled-msft)

(define-condition error-controller-model-key-invalid-msft (simple-error)
  ())
(setf (gethash -1000055000 *error-conditions*) 'error-controller-model-key-invalid-msft)

(define-condition error-reprojection-mode-unsupported-msft (simple-error)
  ())
(setf (gethash -1000066000 *error-conditions*) 'error-reprojection-mode-unsupported-msft)

(define-condition error-compute-new-scene-not-completed-msft (simple-error)
  ())
(setf (gethash -1000097000 *error-conditions*) 'error-compute-new-scene-not-completed-msft)

(define-condition error-scene-component-id-invalid-msft (simple-error)
  ())
(setf (gethash -1000097001 *error-conditions*) 'error-scene-component-id-invalid-msft)

(define-condition error-scene-component-type-mismatch-msft (simple-error)
  ())
(setf (gethash -1000097002 *error-conditions*) 'error-scene-component-type-mismatch-msft)

(define-condition error-scene-mesh-buffer-id-invalid-msft (simple-error)
  ())
(setf (gethash -1000097003 *error-conditions*) 'error-scene-mesh-buffer-id-invalid-msft)

(define-condition error-scene-compute-feature-incompatible-msft (simple-error)
  ())
(setf (gethash -1000097004 *error-conditions*) 'error-scene-compute-feature-incompatible-msft)

(define-condition error-scene-compute-consistency-mismatch-msft (simple-error)
  ())
(setf (gethash -1000097005 *error-conditions*) 'error-scene-compute-consistency-mismatch-msft)

(define-condition error-display-refresh-rate-unsupported-fb (simple-error)
  ())
(setf (gethash -1000101000 *error-conditions*) 'error-display-refresh-rate-unsupported-fb)

(define-condition error-color-space-unsupported-fb (simple-error)
  ())
(setf (gethash -1000108000 *error-conditions*) 'error-color-space-unsupported-fb)

(define-condition error-space-component-not-supported-fb (simple-error)
  ())
(setf (gethash -1000113000 *error-conditions*) 'error-space-component-not-supported-fb)

(define-condition error-space-component-not-enabled-fb (simple-error)
  ())
(setf (gethash -1000113001 *error-conditions*) 'error-space-component-not-enabled-fb)

(define-condition error-space-component-status-pending-fb (simple-error)
  ())
(setf (gethash -1000113002 *error-conditions*) 'error-space-component-status-pending-fb)

(define-condition error-space-component-status-already-set-fb (simple-error)
  ())
(setf (gethash -1000113003 *error-conditions*) 'error-space-component-status-already-set-fb)

(define-condition error-unexpected-state-passthrough-fb (simple-error)
  ())
(setf (gethash -1000118000 *error-conditions*) 'error-unexpected-state-passthrough-fb)

(define-condition error-feature-already-created-passthrough-fb (simple-error)
  ())
(setf (gethash -1000118001 *error-conditions*) 'error-feature-already-created-passthrough-fb)

(define-condition error-feature-required-passthrough-fb (simple-error)
  ())
(setf (gethash -1000118002 *error-conditions*) 'error-feature-required-passthrough-fb)

(define-condition error-not-permitted-passthrough-fb (simple-error)
  ())
(setf (gethash -1000118003 *error-conditions*) 'error-not-permitted-passthrough-fb)

(define-condition error-insufficient-resources-passthrough-fb (simple-error)
  ())
(setf (gethash -1000118004 *error-conditions*) 'error-insufficient-resources-passthrough-fb)

(define-condition error-unknown-passthrough-fb (simple-error)
  ())
(setf (gethash -1000118050 *error-conditions*) 'error-unknown-passthrough-fb)

(define-condition error-render-model-key-invalid-fb (simple-error)
  ())
(setf (gethash -1000119000 *error-conditions*) 'error-render-model-key-invalid-fb)

(define-condition error-marker-not-tracked-varjo (simple-error)
  ())
(setf (gethash -1000124000 *error-conditions*) 'error-marker-not-tracked-varjo)

(define-condition error-marker-id-invalid-varjo (simple-error)
  ())
(setf (gethash -1000124001 *error-conditions*) 'error-marker-id-invalid-varjo)

(define-condition error-spatial-anchor-name-not-found-msft (simple-error)
  ())
(setf (gethash -1000142001 *error-conditions*) 'error-spatial-anchor-name-not-found-msft)

(define-condition error-spatial-anchor-name-invalid-msft (simple-error)
  ())
(setf (gethash -1000142002 *error-conditions*) 'error-spatial-anchor-name-invalid-msft)

(define-condition error-space-mapping-insufficient-fb (simple-error)
  ())
(setf (gethash -1000169000 *error-conditions*) 'error-space-mapping-insufficient-fb)

(define-condition error-space-localization-failed-fb (simple-error)
  ())
(setf (gethash -1000169001 *error-conditions*) 'error-space-localization-failed-fb)

(define-condition error-space-network-timeout-fb (simple-error)
  ())
(setf (gethash -1000169002 *error-conditions*) 'error-space-network-timeout-fb)

(define-condition error-space-network-request-failed-fb (simple-error)
  ())
(setf (gethash -1000169003 *error-conditions*) 'error-space-network-request-failed-fb)

(define-condition error-space-cloud-storage-disabled-fb (simple-error)
  ())
(setf (gethash -1000169004 *error-conditions*) 'error-space-cloud-storage-disabled-fb)

(define-condition error-hint-already-set-qcom (simple-error)
  ())
(setf (gethash -1000306000 *error-conditions*) 'error-hint-already-set-qcom)


