(in-package #:3b-openxr-wrappers)

;;; 10. Rendering

;; 10.1. Swapchain Image Management

(defun enumerate-swapchain-formats (session)
  (with-two-call (i o p :int64)
    (%:enumerate-swapchain-formats session i o p)))


(defun create-swapchain(session .format .width .height
                        &key (sample-count 1)
                          (face-count 1) (array-size 1) (mip-count 1)
                          (create-flags ())
                          (usage-flags '(:color-attachment)))
  (with-swapchain-create-info (sci
                               :create-flags create-flags
                               :usage-flags usage-flags
                               :format .format
                               :sample-count sample-count
                               :width .width
                               :height .height
                               :face-count face-count
                               :array-size array-size
                               :mip-count mip-count)
    (cffi:with-foreign-object (p '%:swapchain)
      (setf (cffi:mem-ref p :uint32) #xdead)
      (check-result (%:create-swapchain session sci p))
      (let ((s (cffi:mem-ref p '%:swapchain)))
        (list :handle s
              :width (cffi:foreign-slot-value
                      sci '(:struct %:swapchain-create-info) '%:width)
              :height (cffi:foreign-slot-value
                       sci '(:struct %:swapchain-create-info) '%:height))))))

(defun create-swapchain/gl (session format width height
                            &rest r
                            &key (sample-count 1)
                              (face-count 1) (array-size 1) (mip-count 1)
                              (create-flags ())
                              (usage-flags '(:color-attachment)))
  (declare (ignorable sample-count face-count array-size mip-count create-flags usage-flags))
  (let* ((s (apply #'create-swapchain session format width height r))
         (h (getf s :handle)))
    (list* :images (coerce
                    (loop for i in (enumerate-swapchain-images/gl h)
                          collect (getf i '%:image))
                    'vector)
           s)))


(import-export %:destroy-swapchain)

(defun enumerate-swapchain-images/gl (swapchain)
  (with-two-call (i o p (:struct %:swapchain-image-opengl-khr))
    (%:enumerate-swapchain-images swapchain i o p)))

;; no enumerate-swapchain-images for now since it needs a specific
;; type

(defun acquire-swapchain-image (swapchain)
  ;; not actually required, unless there is some extension struct to
  ;; put in :next.
  (with-swapchain-image-acquire-info (siai)
    (with-returned-atom (p :uint32)
      (%:acquire-swapchain-image swapchain siai p))))

(defun wait-swapchain-image (swapchain &key (timeout 0))
  (with-swapchain-image-wait-info (siwi :timeout timeout)
    (check-result (%:wait-swapchain-image swapchain siwi))))

(defun release-swapchain-image (swapchain)
  ;; empty struct, so could be null pointer instead
  (with-swapchain-image-release-info (siri)
    (%:release-swapchain-image swapchain siri)))

(defmacro with-swapchain-image/1 ((index-var swapchain)
                                  &body body)
  ;; todo: add timeout options (might need more complicated setup
  ;; though, since we need to not acquire (too many) more images if we
  ;; haven't successfully waited on the ones we already have.
  (a:once-only (swapchain)
    ;; error-call-order-invalid here indicates we have acquired all
    ;; the images without wait+release
    `(let ((,index-var (acquire-swapchain-image ,swapchain)))
       (let ((ok (unqualified-success
                  (wait-swapchain-image ,swapchain :timeout 0))))
         (when ok
           (unwind-protect
                (progn ,@body)
             (release-swapchain-image ,swapchain)))))))


;; 10.2. View and Projection State

;; combination of xrView, xrCompositionLayerProjectionView, and
;; xrViewState, used as output from locate-views and input to
;; end-frame, with some extra fields for user drawing cod
(defstruct projection-view
  ;; *-view pose (or nil if ViewState -valid flag wasn't set)
  (position nil :type (or null (simple-array single-float (3))))
  (orientation nil :type (or null (simple-array single-float (4))))
  ;; ViewState flags
  (position-tracked nil :type boolean)
  (orientation-tracked nil :type boolean)
  ;; *-view `fov`
  (fov (make-fov) :type fov)
  ;; composition-layer-projection-view sub-image
  (index 0 :type (unsigned-byte 32))
  (swapchain 0 :type (unsigned-byte 64))
  ;; composition-layer-projection-view sub-image image-rect
  (image-top 0 :type (signed-byte 32))
  (image-bottom 0 :type (signed-byte 32))
  (image-left 0 :type (signed-byte 32))
  (image-right 0 :type (signed-byte 32))
  ;; current image, for user code
  (image 0 :type (unsigned-byte 64)))

(defun locate-views (session at view-config space)
  (with-view-locate-info (vli :view-configuration-type view-config
                              :display-time at
                              :space space)
    (with-view-state (vs :%slots t)
      (loop with views = (with-two-call (i o p (:struct %:view))
                           (%:locate-views session vli vs i o p))
            ;; this needs to be after call to %:locate-views
            with f = %:view-state-flags
            for v in views
            for fov = (getf v '%:fov)
            for pose = (getf v '%:pose)
            for pos = (getf pose '%:position)
            for or = (getf pose '%:orientation)
            ;; todo: move view state etc into some global object to
            ;; reduce consing here?
            collect (%::with-bits (f %:view-state-flags)
                      (make-projection-view
                       :position (when (f :position-valid) pos)
                       :orientation (when (f :orientation-valid) or)
                       :orientation-tracked (f :orientation-tracked)
                       :position-tracked (f :position-tracked)
                       :fov (make-fov :up (getf fov '%:angle-up)
                                      :down (getf fov '%:angle-down)
                                      :left (getf fov '%:angle-left)
                                      :right (getf fov '%:angle-right))))))))

;; 10.3. Frame Synchronization

(defun wait-frame (session)
  (with-frame-wait-info (fwi)
    (with-frame-state (fs :%slots t)
      (check-result (%:wait-frame session fwi fs))
      (values (not (zerop %:should-render))
              %:predicted-display-time %:predicted-display-period))))

;; 10.4. Frame Submission

(defun begin-frame (session)
  (with-frame-begin-info (fbi)
    (check-result (%:begin-frame session fbi))))

(defun %translate-projection-view (l p)
  (cffi:with-foreign-slots ((%:type
                             %:next
                             (pose :pointer %:pose)
                             (fov :pointer %:fov)
                             (sub-image :pointer %:sub-image))
                            p (:struct %:composition-layer-projection-view))
    (setf %:type :type-composition-layer-projection-view
          %:next (cffi:null-pointer))
    (cffi:with-foreign-slots (((orientation :pointer %:orientation)
                               (position :pointer %:position))
                              pose
                              (:struct %:pose-f))
      (setf (cffi:mem-ref position '(:struct %:vector-3f))
            (or (projection-view-position l) #(0f0 0f0 0f0))
            (cffi:mem-ref orientation '(:struct %:quaternion-f))
            (or (projection-view-orientation l)
                #(0f0 0f0 0f0 1f0))))

    (cffi:with-foreign-slots ((%:angle-left
                               %:angle-right
                               %:angle-up
                               %:angle-down)
                              fov (:struct %:fov-f))
      (setf %:angle-up (angle-up (projection-view-fov l))
            %:angle-down (angle-down (projection-view-fov l))
            %:angle-left (angle-left (projection-view-fov l))
            %:angle-right (angle-right (projection-view-fov l))))
    (cffi:with-foreign-slots (((image-rect :pointer %:image-rect)
                               %:image-array-index
                               %:swapchain)
                              sub-image (:struct %:swapchain-sub-image))
      (setf %:image-array-index 0
            %:swapchain (projection-view-swapchain l))
      (cffi:with-foreign-slots (((offset :pointer %:offset)
                                 (extent :pointer %:extent))
                                image-rect (:struct %:rect-2d-i))
        (cffi:with-foreign-slots ((%:width %:height)
                                  extent (:struct %:extent-2d-i))
          (cffi:with-foreign-slots ((%:x %:y)
                                    offset (:struct %:offset-2d-i))
            (setf %:x (projection-view-image-left l)
                  %:y (projection-view-image-top l)
                  %:width (- (projection-view-image-right l)
                             (projection-view-image-left l))
                  %:height (- (projection-view-image-bottom l)
                              (projection-view-image-top l)))))))))


(defun end-frame (session space display-time views &key (blend-mode :opaque))
  (let ((n (length views)))
    (cffi:with-foreign-object (clpv '(:struct %:composition-layer-projection-view) n)
      (loop for i below n
            for v = (aref views i)
            for p1 = (cffi:mem-aptr clpv '(:struct %:composition-layer-projection-view) i)
            do (%translate-projection-view v p1))
      (with-composition-layer-projection (clp
                                          :layer-flags ()
                                          :space space
                                          :view-count n
                                          :views clpv)
        (cffi:with-foreign-object (p :pointer)
          (setf (cffi:mem-ref p :pointer) clp)
          (with-frame-end-info (fei :display-time display-time
                                    :environment-blend-mode blend-mode
                                    :layer-count 1
                                    :layers p)
            (float-features:with-float-traps-masked (:overflow)
              (check-result (%:end-frame session fei)))))))))

(defmacro with-frame ((session space display-time layers-var) &body body)
  ;; todo: blend-mode, layers args
  (a:once-only (session)
    `(let ((,layers-var (make-array 2 :adjustable t :fill-pointer 0)))
       (begin-frame ,session)
       ;; entering debugger tends to cause further errors since
       ;; DISPLAY-TIME is usually past by the time we are done
       ;; debugging, so add option to skip the rest of the frame,
       ;; including END-FRAME. (Calling BEGIN-FRAME twice without
       ;; END-FRAME is valid, and should just cause a FRAME-DISCARDED
       ;; return from second BEGIN-FRAME call.)
       (with-simple-restart (continue "Skip rest of frame")
         ;; no UNWIND-PROTECT so restart can skip END-FRAME
         ,@body
         (end-frame ,session ,space ,display-time ,layers-var)))))


(defun enumerate-environment-blend-modes (system-id view-config)
  (with-two-call (i o p %:environment-blend-mode)
    (%:enumerate-environment-blend-modes
     *instance* system-id view-config i o p)))
