#++
(ql:quickload '(3b-openxr
                ;; we need OpenGL
                cl-opengl
                ;; we need some way to get a GL context and some
                ;; window state, SDL or glfw should also work
                glop
                ;; we need a 3d matrix library
                sb-cga
                ;; we need to disable floating point traps on some
                ;; runtimes
                float-features))
(defpackage #:3b-openxr-example
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    ;; main wrapper API package
                    (#:xr #:3b-openxr)
                    ;; low-level bindings. shouldn't need this much,
                    ;; but still need a few symbols currently
                    (#:%xr #:3b-openxr-bindings)
                    ;; might be useful if we want different behavior
                    ;; from the wrappers, or want to optimize
                    (#:xm #:3b-openxr-mid-level)))
(in-package #:3b-openxr-example)
;;; Without an instance, you can query available layers and extensions
;;; with
;;  (xr:enumerate-api-layer-properties)
;;; and
;; (xr:enumerate-instance-extension-properties nil)
;;; respectively. (If you don't have it already from installing the XR
;;; SDK / loader, you can get the validation layer and some others
;;; from the OpenXR-SDK-Source repo on github:
;; https://github.com/KhronosGroup/OpenXR-SDK-Source/releases
;;; (You may need to set `XR_API_LAYER_PATH` environment variable to
;;; the directory containing the layers.)


(xr:enumerate-api-layer-properties)
#+->
NIL

#++
(sb-posix:setenv "XR_API_LAYER_PATH" "/some/path/to/openxr_loader_windows/x64/bin/api_layers" 0)

#++
(xr:enumerate-api-layer-properties)
#+->
((:NAME "XR_APILAYER_LUNARG_api_dump" :VERSION 1 :SPEC-VERSION (1 0 0)
  :DESCRIPTION "API Layer to record api calls as they occur")
 (:NAME "XR_APILAYER_LUNARG_core_validation" :VERSION 1 :SPEC-VERSION (1 0 0)
  :DESCRIPTION
        "API Layer to perform validation of api calls and parameters as they occur"))

(xr:enumerate-instance-extension-properties nil)
#+->
((:NAME "XR_KHR_vulkan_enable" :VERSION 8)
 (:NAME "XR_KHR_vulkan_enable2" :VERSION 2)
 (:NAME "XR_KHR_D3D11_enable" :VERSION 9)
 (:NAME "XR_KHR_D3D12_enable" :VERSION 9)
 (:NAME "XR_KHR_opengl_enable" :VERSION 10)
 (:NAME "XR_KHR_win32_convert_performance_counter_time" :VERSION 1)
 (:NAME "XR_EXT_win32_appcontainer_compatible" :VERSION 1)
 (:NAME "XR_KHR_binding_modification" :VERSION 1)
 (:NAME "XR_KHR_composition_layer_depth" :VERSION 6)
 (:NAME "XR_KHR_visibility_mask" :VERSION 2)
 (:NAME "XR_EXT_active_action_set_priority" :VERSION 1)
 (:NAME "XR_EXT_dpad_binding" :VERSION 1)
 (:NAME "XR_EXT_frame_composition_report" :VERSION 4)
 (:NAME "XR_EXT_hand_tracking" :VERSION 4)
 (:NAME "XR_EXT_hand_joints_motion_range" :VERSION 1)
 (:NAME "XR_EXT_hp_mixed_reality_controller" :VERSION 1)
 (:NAME "XR_EXT_palm_pose" :VERSION 2)
 (:NAME "XR_FB_display_refresh_rate" :VERSION 1)
 (:NAME "XR_HTC_vive_cosmos_controller_interaction" :VERSION 1)
 (:NAME "XR_HTC_vive_focus3_controller_interaction" :VERSION 2)
 (:NAME "XR_MND_headless" :VERSION 2)
 (:NAME "XR_VALVE_analog_threshold" :VERSION 2)
 (:NAME "XR_HTCX_vive_tracker_interaction" :VERSION 2)
 (:NAME "XR_EXT_debug_utils" :VERSION 4))

;;; you can also query whether a layer adds any extensions (no, in this case)
#++
(xr:enumerate-instance-extension-properties "XR_APILAYER_LUNARG_core_validation")
#+->
NIL


;;; some globals we will use to avoid a bunch of nested LETs
(defvar *system*)
(defvar *views*)
(defvar *session*)
(defvar *reference-type*)
(defvar *space*)
(defvar *format*)
(defvar *hand*)
(defvar *action-set*)
(defvar *teleport*)
(defvar *haptics*)
(defvar *aim*)
(defvar *aim-space*)
(defvar *swapchains*)
(defvar *fbo*)
(defvar *window*)
(defvar *xr-state*)
#++(main)
(defun main ()
  ;; like many C APIs, OpenXR runtimes tend to assume float traps are
  ;; off. 3b-openxr doesn't currently disable them per call, so just
  ;; wrap the whole thing:

  (float-features:with-float-traps-masked (:invalid)
    (let (*system*
          *views*
          *session*
          *reference-type*
          *space*
          *format*
          *hand*
          *action-set*
          *aim*
          *teleport*
          *haptics*
          *aim-space*
          *fbo*
          (*xr-state* :idle))

;;; Other than that, pretty much everything else needs an
;;; instance. Currently you need to do everything within
;;; `xr:with-instance` for that, since loading extensions depends on
;;; some specials to find the instance and current extension
;;; pointers. At some point there might be a more functional API for
;;; that, and will definitely need some way to handle running things
;;; on other threads. Currently using XR from multiple threads would
;;; require some hacking to make the extension loader work with
;;; threads.

;;; When creating an instance, specify lists of extensions and layers
;;; to enable, along with description of the application (and
;;; optionally engine). You can also enable debug logging from the XR
;;; `Debug Utils` extension if available.

      (xr:with-instance (:extensions
                         ;; real code should check for availability of
                         ;; specific extensions before enabling and using
                         ;; them, but hard coded here to simplify
                         ;; code. (These constants are just strings, you can
                         ;; use those directly if you prefer)
                         (list
                          ;; adds callbacks for various debug/info/etc messages
                          %xr:ext-debug-utils-extension-name ;; "XR_EXT_debug_utils"
                          ;; adds hand/finger tracking
                          %xr:ext-hand-tracking-extension-name
                          ;; adds support for opengl
                          %xr:khr-opengl-enable-extension-name)

                         :layers
                         ;; validation layer, does runtime checks for
                         ;; incorrect API usage and sends messages to the
                         ;; debug callback (or to stdout or a file if
                         ;; configured to do so)
                         (when (xr:layer-available-p
                                "XR_APILAYER_LUNARG_core_validation")
                           (list "XR_APILAYER_LUNARG_core_validation"))

                         ;; required
                         :application-name "testing"
                         ;; versions and engine name are optional
                         :application-version 1 ;; unsigned-byte 32
                         :engine-name "some-engine"
                         :application-version 2 ;; unsigned-byte 32

                         ;; we can also specify the XR API version we
                         ;; expect.  default is whatever the bindings were
                         ;; built with, but you can override it if you
                         ;; need a specific version, or don't want it to
                         ;; automatically change from what you tested when
                         ;; bindings are updated.
                         :api-version (xr:make-version 1 0 27)

                         ;; enable debug callback for all messages. This
                         ;; might change a bit once callbacks to
                         ;; user-provided CL functions are implemented.
                         :message-severities '(:error-ext :warning-ext
                                               :verbose-ext :info-ext)
                         :message-types '(:general-ext :validation-ext
                                          :performance-ext :conformance-ext)
                         ;; this API isn't final, so using an internal symbol
                         ;; for now. Eventually should be able to specify a CL
                         ;; function designator and the internal function will
                                        ; ;call it.
                         :user-callback (cffi:get-callback
                                         '3b-openxr-wrappers::debug-messenger-callback))

;;; Once we have an instance, we can query a few more details about
;;; the runtime.
        (format t "instance properties = ~s~%" (xr:get-instance-properties))

;;; ->
;;; instance properties = (:RUNTIME-NAME "SteamVR/OpenXR" :VERSION (0 1 0))



;;; next we have to create/configure a bunch of things more or less in order:

;;; first is the selecting a 'system ID', which represents a set of
;;; hardware for a particular style of VR/AR. We can choose from
;;; :head-mounted-display or :handheld-display (or anything else
;;; provided by enabled extensions). Not sure there is any way to
;;; query what's available, but usually we will have written for
;;; something specific and just hard-code it.

        ;; for cases like this where failures are expected, we can use
        ;; ignore specific conditions
        (let ((handheld (xr:ignore-xr-error (xr:error-form-factor-unsupported)
                          (xr:get-system :handheld-display))))
          (format t "handheld display = ~x~%" handheld))

        ;; this code assumes a HMD, so don't bother catching the
        ;; error. Real code might want to handle
        ;; XR:ERROR-FORM-FACTOR-UNAVAILABLE (supported, but not available
        ;; right now) and give users an option to try again after they
        ;; plug it in or whatever.)
        (setf *system* (xr:get-system :head-mounted-display))

        ;; we can query some more info once we have a specific system
        (format t "hmd = #x~x~%" *system*)
        ;; xr:get-system-properties (and xr:get-system-*-properties-*)
        ;; provide some info about the specific hardware we are using, and
        ;; its features. In particular, this call tells us we have
        ;; tracking for position, orientation, and hand.
        (format t " hand tracking properties =~%  ~s ~%"
                ;; equivalent to xr:get-system-properties, with additional
                ;; fields for the hand tracking extension. Currently you
                ;; can only query system-properties for up to 1 extension
                ;; at a time.
                (xr:get-system-hand-tracking-properties-ext *system*))

        ;; and finally, we can ask what the requirements the runtime has
        ;; for the graphics library we plan to use. Note that calling this
        ;; is required before creating a session, or you will get an
        ;; XR:ERROR-GRAPHICS-REQUIREMENTS-CALL-MISSING error
        (format t " Opengl requirements: ~s~%"
                (xr:get-opengl-graphics-requirements-khr *system*))
;;; ->
;;; handheld display = NIL
;;; hmd = #x1000785C000000E8
;;;  hand tracking properties =
;;;   (:SUPPORTS-HAND-TRACKING T
;;;    :SYSTEM-ID 1153053841139171560
;;;    :VENDOR-ID 10462
;;;    :SYSTEM-NAME "SteamVR/OpenXR : lighthouse"
;;;    :MAX-SWAPCHAIN-IMAGE-WIDTH 2468
;;;    :MAX-SWAPCHAIN-IMAGE-HEIGHT 2740
;;;    :MAX-LAYER-COUNT 16
;;;    :POSITION-TRACKING T
;;;    :ORIENTATION-TRACKING T)
;;;  Opengl requirements: (:MIN-API (4 3 0) :MAX-API (4 6 0))


;;; We also need to pick a 'view configuration', and get some
;;; configuration info from that.

        ;; XR supports a few different screen configurations, for example
        ;; a single view for a handheld AR display, or 2 views for a
        ;; stereo hmd.
        (format t " view configurations: ~s~%"
                (xr:enumerate-view-configurations *system*))
        ;; We can check properties of a view configuration:
        ;; We will hard-code :primary-stereo, since we again assume an HMD.
        (format t "   properties: ~s~%"
                (xr:get-view-configuration-properties *system* :primary-stereo))
        ;; We can also ask what resolution the runtime thinks we should
        ;; use for the views (we will use this later, so save the results)
        (setf *views* (xr:enumerate-view-configuration-views *system* :primary-stereo))
        (format t "   views: ~s~%" *views*)
        ;; Depending on what sort of device and view we have, it might
        ;; support various types of blending with environment, either by
        ;; being transparent or blending into a live camera image.
        (format t " blend modes: ~s~%"
                (xr:enumerate-environment-blend-modes *system* :primary-stereo))
;;; ->
;;;  view configurations: (:PRIMARY-STEREO)
;;;    properties: (:PRIMARY-STEREO :FOV-MUTABLE T)
;;;    views: ((:RECOMMENDED-IMAGE-RECT-HEIGHT 2740
;;;             :RECOMMENDED-IMAGE-RECT-WIDTH 2468
;;;             :RECOMMENDED-SWAPCHAIN-SAMPLE-COUNT 1
;;;             :MAX-IMAGE-RECT-HEIGHT 8192
;;;             :MAX-IMAGE-RECT-WIDTH 8192
;;;             :MAX-SWAPCHAIN-SAMPLE-COUNT 1)
;;;            (:RECOMMENDED-IMAGE-RECT-HEIGHT 2740
;;;             :RECOMMENDED-IMAGE-RECT-WIDTH 2468
;;;             :RECOMMENDED-SWAPCHAIN-SAMPLE-COUNT 1
;;;             :MAX-IMAGE-RECT-HEIGHT 8192
;;;             :MAX-IMAGE-RECT-WIDTH 8192
;;;             :MAX-SWAPCHAIN-SAMPLE-COUNT 1))
;;;  blend modes: (:OPAQUE)


;;; next is the 'session', which tells the runtime we want to actually
;;; start using the device. Creating a session requires some info from
;;; the windowing system/OS and the graphics API, so use SDL,GLFW,GLOP
;;; etc to create a window and ask them for the right values. (there
;;; is a 'headless' extension for creating a session without the extra
;;; info, but that can't actually display anything, so not too
;;; interesting here.)

        (glop:with-window (*window* "OpenXR test" 800 600 :x -1000 :y 32)
          ;; make sure vsync is off if you plan to draw anything to
          ;; the window from same thread
          (glop::swap-interval *window* 0)
          ;; for now only windows+opengl is supported, so we need an HDC
          ;; and an HGLRC. WITH-SESSION takes care of calling
          ;; xr:destroy-session for us later.
          (xr:with-session (*session* *system*
                                      ;; tell 3b-openxr which extension we
                                      ;; are using (see xr:create-session
                                      ;; for what other options will be
                                      ;; when implemented)
                                      :gl-win32
                                      :hdc (glop::win32-window-dc *window*)
                                      :hglrc (glop::wgl-context-ctx
                                              (glop::window-gl-context
                                               *window*)))
            ;; for debugging, we can mark regions of code with labels that
            ;; might show up in the callbacks to hopefully make debugging
            ;; easier.
            (xr:with-debug-utils-label (*session* "session")

;;; once we have a session, we can once again query some more info. We
;;; will also need to pick one of the options from each of these for
;;; later use.

              ;; the reference space is the coordinate system of our 'world':
              (let ((spaces (xr:enumerate-reference-spaces *session*)))
                (format t "reference spaces: ~s~%" spaces)
                ;; :stage is a "room-scale" bounded area, so prefer that.
                ;; :local and :local-floor-ext are "seated" or "in-place
                ;; standing", so try those if stage isn't available. :view
                ;; is locked to the display so wouldn't be very good for
                ;; main view, but might be useful for HUDs etc.
                (loop for i in '(:stage :local-floor-ext :local :view)
                      when (member i spaces)
                        do (setf *reference-type* i)
                           (loop-finish))
                (format t "  using ~s~%" *reference-type*))

              ;; we need to decide what format will be used for the
              ;; textures into which we will be drawing for display, so
              ;; get a list of the ones the runtime supports:
              (let ((formats (xr:enumerate-swapchain-formats *session*)))
                (format t "swapchain formats: ~s~%" formats)
                ;; that returns gl enums since we created a gl
                ;; session, but they are integers since 3b-openxr
                ;; doesn't currently track enough info to know they
                ;; are GL enums (and doesn't depend on cl-opengl, so
                ;; wouldn't know how to translate them anyway)
                (format t " = : ~s~%"
                        (loop for i in formats
                              collect (cffi:foreign-enum-keyword '%gl:enum i)))
                ;; and pick one from the list (they are supposed to be in
                ;; more or less in runtime's preferred order, so picking the
                ;; first is also reasonable if we don't have any preference)
                (setf *format*
                      (or
                       (loop ;; we will prefer 16bit fp, for no particular reason
                             for i in '(:rgba16f :rgb16f)
                             for n = (cffi:foreign-enum-value '%gl:enum i)
                             ;; compare by numeric value since symbols are
                             ;; ambiguous
                             when (member n formats)
                               return n)
                       ;; but fall back to whatever is supported
                       (car formats)))
                (format t "using ~s (~s)~%" *format*
                        (cffi:foreign-enum-keyword '%gl:enum *format*)))

;;; ->
;;; reference spaces: (:VIEW :LOCAL :STAGE)
;;;   using :STAGE
;;; swapchain formats: (32859 34842 34843 35905 35907 33189 33190 36012)
;;;  = : (:RGBA16-EXT :RGBA16F-EXT :RGB16F-EXT :SRGB8-NV :SRGB8-ALPHA8-EXT
;;;       :DEPTH-COMPONENT16-SGIX :DEPTH-COMPONENT24-SGIX :DEPTH-COMPONENT32F)
;;; using 34842 (:RGBA16F-EXT)

;;; In addition to the scoped xr:with-debug-utils-label, we can also
;;; add temporary labels that last until the next temporary label, or
;;; the next time we enter or leave a label scope:
              (xr:session-insert-debug-utils-label-ext *session* "input")

;;; We enabled the hand tracking extension, so create an object for
;;; tracking the right hand state, and store it for later.
              (setf *hand* (xr:create-hand-tracker-ext *session* :right-ext))

;;; When using the debug extension, we can attach names to objects
;;; that will be included when messages reference those objects (and
;;; even without the extension, the name will be stored in a wrapper
;;; you can see in debugger)
              (xr:set-debug-utils-object-name-ext *hand* "{right hand tracker}")

;;; we also need to configure input and haptics bindings if we want to
;;; interact with anything once we are in VR.

              ;; OpenXR groups bindings into "Action Sets", so you can
              ;; make a set for gameplay, another for a menu, etc, and
              ;; switch between them as needed.  The main `name` for most
              ;; things is relatively restricted, see first clause of
              ;; Well-Formed Path Strings at
              ;; https://registry.khronos.org/OpenXR/specs/1.0/html/xrspec.html#well-formed-path-strings
              ;; in the spec.
              (let* ((in-game (xr:create-action-set
                               ;; name within the API
                               "gameplay"
                               ;; The `:localized-name` is what will
                               ;; be shown to users the runtime's
                               ;; configuration UI, and defaults to
                               ;; the normal name in the wrappers if
                               ;; not specified.
                               :localized-name "Gameplay"
                               ;; we can have multiple sets active at once,
                               ;; larger numbers override lower where they
                               ;; share an input source.
                               :priority 0
                               ;; we can also set the debug name of
                               ;; objects as they are created
                               :object-name "in-game actions"))
                     ;; within an action set, we have a set of actions for input

                     ;; :pose-input lets us track an orientation+position
                     ;; state continuously

                     ;; :boolean-input is an on/off flag (trigger, button,
                     ;; etc)

                     ;; there are also float (analog triggers, etc) and
                     ;; vector2 (dpad, touchpad position, etc) inputs

                     (aim (xr:create-action in-game "aim" :pose-input))
                     (teleport (xr:create-action in-game "teleport" :boolean-input))
                     ;; and output
                     (haptics (xr:create-action in-game "player-hit"
                                                :vibration-output
                                                :localized-name "Player Hit")))

                ;; to actually use the action set, we need to tell XR how
                ;; we think they should map to various types of
                ;; hardware.
                (xr:suggest-interaction-profile-bindings
                 ;; things that accept paths want them translated to xr
                 ;; atoms (possibly 3b-openxr should do this
                 ;; automatically when given strings, but it doesn't
                 ;; currently)
                 (xr:string-to-path "/interaction_profiles/valve/index_controller")
                 teleport (xr:string-to-path "/user/hand/right/input/trigger/click")
                 haptics (xr:string-to-path "/user/hand/right/output/haptic")
                 aim (xr:string-to-path "/user/hand/left/input/aim"))
                #++
                (xr:suggest-interaction-profile-bindings
                 (xr:string-to-path "/interaction_profiles/htc/vive_controller")
                 ;;...
                 )
                #++
                (xr:suggest-interaction-profile-bindings
                 (xr:string-to-path "/interaction_profiles/khr/simple_controller")
                 ;;...
                 )
                #++
                (xr:suggest-interaction-profile-bindings
                 (xr:string-to-path "/interaction_profiles/microsoft/motion_controller")
                 ;;...
                 )
                #++
                (xr:suggest-interaction-profile-bindings
                 (xr:string-to-path "/interaction_profiles/oculus/touch_controller")
                 ;;...
                 )
                ;; and a few more, and then some more if various
                ;; extensions are present

                ;; we also need to create a `space` for tracking the
                ;; `:pose-input` `aim`
                (setf *aim-space* (xr:create-action-space *session* aim))
                ;; and we need to attach the action sets to the session
                (xr:attach-session-action-sets *session* (vector in-game))

                ;; save the action set and actions for later
                (setf *action-set* (vector in-game))
                (setf *teleport* teleport
                      *haptics* haptics
                      *aim* aim))

;;; we also need to create a space for the reference space we chose
;;; earlier
              (xr:with-reference-space (*space* *session* *reference-type*)

;;; the last thing we need to do before starting the main loop is
;;; create "swapchains", which are sets of images that will cycle
;;; between being drawn into by our application and being sent to the
;;; display by the runtime.
                (xr:session-insert-debug-utils-label-ext *session* "swapchains")

                (let ((*swapchains*
                        ;; we need to create a swapchain for each entry in
                        ;; the views we queried earlier
                        (loop
                          for v in *views*
                          collect (xr:create-swapchain/gl
                                   *session*
                                   ;; use the gl texture format we picked
                                   ;; earlier
                                   *format*
                                   ;; use the suggested image size
                                   (getf v ':recommended-image-rect-width)
                                   (getf v ':recommended-image-rect-height)
                                   ;; and sample count
                                   :sample-count
                                   (getf v ':recommended-swapchain-sample-count)
                                   ;; there are a bunch of other flags you
                                   ;; might want if you are writing more
                                   ;; complicated shaders.
                                   :usage-flags '(:color-attachment :sampled)))))

                  ;; create ab FBO for GL to render into
                  (let ((fb (gl:create-framebuffer))
                        (db (gl:create-renderbuffer))
                        ;; use size of largest swapchain image we
                        ;; created above (though probably they are the
                        ;; same size)
                        (w (loop for i in *swapchains*
                                 maximize (xr:swapchain-width i)))
                        (h (loop for i in *swapchains*
                                 maximize (xr:swapchain-height i))))
                    (format t "creating fbo ~sx~s~%" w h)
                    (%gl:named-renderbuffer-storage db :depth-component24 w h)
                    (%gl:named-framebuffer-renderbuffer fb :depth-attachment :Renderbuffer db)
                    (setf *fbo* (list fb db)))

                  ;; and we can finally start the main loop (defined
                  ;; below)
                  (xr:with-debug-utils-label (*session* "main-loop")
                    (main-loop))

                  ;; clean up the FBO
                  (when *fbo*
                    (gl:bind-framebuffer :draw-framebuffer 0)
                    (gl:delete-renderbuffer (second *fbo*))
                    (gl:delete-framebuffer (first *fbo*))
                    (setf *fbo* nil))

                  ;; when we are done we should destroy the swapchains
                  (map 'nil 'xr:destroy-swapchain
                       (shiftf *swapchains* nil)))))))))))

;; variable so we can exit main loop if it is running but not getting
;; input for some reason
(defparameter *run* nil)
(defun main-loop ()
;;; fairly normal main loop
  (setf *run* t)
  (loop
    ;; store some info to print out fps periodically
    with start = (get-internal-real-time)
    with frames = 0
    for now = (get-internal-real-time)
    ;; check for events from the windowing system (make sure it
    ;; doesn't block)
    while (glop:dispatch-events *window* :blocking nil)
    do
;;; openxr specific parts, broken out into a few more functions

       ;; update state of action sets we configured
       (update-actions)
       ;; poll for events, and update XR state machine
       (dispatch-xr-events)
       ;; in states where we are expected to draw, do so (state
       ;; machine won't advance past :READY unless we are actually
       ;; processing frames)
       (when (member *xr-state* '(:ready
                                  :synchronized
                                  :visible
                                  :focused))
         (draw-xr-frame))
;;; generic stuff

       ;; draw something to the window (make sure vsync is off if
       ;; doing this in same thread as XR rendering, since we want XR
       ;; to control frame rate)
       (with-simple-restart (continue "continue")
         (draw-desktop-frame))
       ;; print frame rate
       (incf frames)
       (when (> (- now start)
                (* 10 internal-time-units-per-second))
         (let ((d (- now start)))
           (format t "~s frames in ~s sec = ~s fps (~s ms)~%"
                   frames
                   (float (/ d internal-time-units-per-second))
                   (float (/ frames
                             (/ d internal-time-units-per-second)))
                   (float (/ (/ d internal-time-units-per-second)
                             frames)))
           (setf start now
                 frames 0)))
    unless *run*
      do (glop:close-window *window*)
         (loop-finish)))

(defun update-actions ()
  (xr:with-debug-utils-label (*session* "update-actions")

;;; input is only available when state is :focused
    (when (or (eql *xr-state* :focused))
      ;; first call sync-actions to update the action set(s)
      (xr:sync-actions *session* *action-set*)
      ;; then we can check the state of the actions (:pose actions are
      ;; queried differently, we will look at those while drawing)
      (let ((tp (xr:get-action-state-boolean *session* *teleport*)))
        ;; currently a plist
        (when (getf tp :changed)
          ;; and do something when the state changed (we will just
          ;; print some state and send some haptics)
          (format t " teleport state -> ~s~%" tp)

          ;; when things are running, you can query which interaction
          ;; profile is in use. There are also events when it changes,
          ;; if you want to change things depending on which
          ;; controller is in use
          (let ((ip (xr:get-current-interaction-profile
                     *session* (xr:string-to-path
                                "/user/hand/right"))))
            (format t "right hand interaction profile = ~s~% = ~s~%"
                    ip (xr:path-to-string ip)))
          ;; and query how things are actually configured
          (let ((s (xr:enumerate-bound-sources-for-action *session* *teleport*)))
            (format t "sources for teleport: ~s~% = ~s~%"
                    s
                    (mapcar 'xr:path-to-string s))
            (format t " = ~s~%"
                    (mapcar (a:curry #'xr:get-input-source-localized-name *session*)
                            s)))
          ;; if we loaded the refresh rate extension, we could query
          ;; that too
          #++
          (format t "refresh rates = ~s~% currently ~s~%"
                  (xr:enumerate-display-refresh-rates-fb *session*)
                  (xr:get-display-refresh-rate-fb *session*))

          ;; we can also send our own messages to the debug callbacks
          (xr:submit-debug-utils-message-ext "debug message test"
                                             :message-id "debug-test"
                                             :function-name "update-actions"
                                             :severity :info-ext)

          (when (getf tp :current)
            (xr:apply-haptic-feedback *session*
                                      *haptics*
                                      :frequency 200.0
                                      :amplitude 0.5
                                      ;; XR time is in nanoseconds or
                                      ;; something, so convert from
                                      ;; seconds to make it readable
                                      :duration (xr:seconds-to-time 0.05))))))))

(defun dispatch-xr-events ()
  ;;https://registry.khronos.org/OpenXR/specs/1.0/html/xrspec.html#events

;;; main API for the XR event loop is currently xr:poll-event-case,
;;; which will bind the slots of the event within the clauses (names
;;; for more common slots are reexported from xr:, but others might
;;; need %xr: until they are added. File issues or PRs for any you
;;; need sooner)
  (xr:poll-event-case ()

;;; the most important event to handle is :session-state-change, since
;;; you need to call xr:begin-session when :ready, and xr:end-session
;;; when :stopping, and probably also exit your application when
;;; :stopping or :exiting. Optionally, you might also want to try to
;;; recover after losing a session, etc.
    (:session-state-changed
     (format t "session state changed ~x -> ~s @ ~s~%"
             xr:session xr:state xr:time)
     (setf *xr-state* xr:state)
     (case xr:state
       (:idle
        ;; The initial state after calling xrCreateSession or
        ;; returned to after calling xrEndSession.
        )
       (:ready
        ;; The application is ready to call xrBeginSession and
        ;; sync its frame loop with the runtime.
        (xr:begin-session *session* :primary-stereo))
       (:synchronized
        ;; The application has synced its frame loop with the
        ;; runtime but is not visible to the user.
        )
       (:visible
        ;; The application has synced its frame loop with the
        ;; runtime and is visible to the user but cannot receive
        ;; XR input.
        )
       (:focused
        ;; The application has synced its frame loop with the
        ;; runtime, is visible to the user and can receive XR
        ;; input.
        )
       (:stopping
        ;; The application should exit its frame loop and call
        ;; xrEndSession.
        (xr:end-session *session*)
        (glop:push-close-event *window*))
       (:loss-pending
        ;; The session is in the process of being lost. The
        ;; application should destroy the current session and
        ;; can optionally recreate it.
        (glop:push-close-event *window*))
       (:exiting
        ;; The application should end its XR experience and not
        ;; automatically restart it.
        (glop:push-close-event *window*))))

;;; most of the other events have defaults that just print something
;;; when xr:*check-verbose* is set. Some of the other important ones
;;; are included here for reference

    ;; instance will be lost soon. destroy instance and all related
    ;; objects, then optionally wait until loss-time and retry
    ;; periodically to see if runtime becomes available again
    (:instance-loss-pending
     (format t "session instance loss pending @ ~s~%" xr:loss-time)
     (xr:end-session *session*)
     (glop:push-close-event *window*))

    ;; we haven't been polling events fast enough, and some were
    ;; dropped
    (:events-lost
     (format t "!!events lost! ~s~%" xr:lost-event-count))

    ;; input devices changed
    (:interaction-profile-changed
     (format t "interaction profile changed for session ~x~%"
             xr:session))

    ;; reference space changed (user recentered it or changed bounds,
    ;; etc)
    (:type-event-data-reference-space-change-pending
     (format t "session #x~x reference space change pending @ ~s~%"
             xr:session xr:change-time)
     (format t "  type -> ~s, pose-valid ~s~%"
             xr:reference-space-type xr:pose-valid)
     (format t "  pose in prev space: ~s~%" xr:pose-in-previous-space))))


(defun draw-xr-frame ()
  ;; first we call xr:wait-frame, which will block until it is time to
  ;; draw a new frame, as well as telling us when the next frame will
  ;; be shown. (it also tells us the frame rate, in case we are triple
  ;; buffering or similar and need to start processing for the frame
  ;; after this, which will be displayed at time (+ at period)).
  ;;
  ;; The time AT is in xr time, which can be translated to windows
  ;; performance-counter time with
  ;; xr:convert-time-to-win32-performance-counter-khr. On other
  ;; platforms, xr:convert-time-to-timespec-time-khr will perform the
  ;; same purpose when it is implemented. (Not done yet, since it will
  ;; probably require groveling to determine format of a timespec.)
  (multiple-value-bind (render at period) (xr:wait-frame *session*)
    (declare (ignorable period))
    (xr:with-debug-utils-label (*session* "frame")
      ;; xr:with-frame calls xr:begin-frame to start processing a
      ;; frame, and on normal exit, calls xr:end-frame to submit the
      ;; frame.
      (xr:with-frame (*session* *space* at layers)
        ;; We need to process frames before we are actually visible,
        ;; to convince XR we actually have a working frame loop, so
        ;; the state machine will progress. Once that happens and we
        ;; get to a visible state, WAIT-FRAME will return true for
        ;; RENDER, indicating we should actually start drawing things.
        (when render
          ;; at that point, we need to ask where we should draw for
          ;; each view
          (loop with views = (xr:locate-views *session* at
                                              :primary-stereo *space*)
                for view in views
                for swapchain in *swapchains*
                for images = (xr:swapchain-images swapchain)
                for w = (xr:swapchain-width swapchain)
                for h = (xr:swapchain-height swapchain)
                ;; then try to acquire an image from the corresponding
                ;; swapchain
                do (xr:with-swapchain-image/1 (index swapchain)
                     ;; the VIEW returned from locate-views is also
                     ;; used to describe what XR should send to the
                     ;; display in the call to END-FRAME inside of
                     ;; WITH-FRAME, so set some values it needs (and
                     ;; for drawing as well)
                     (setf (xr:projection-view-image view) (aref images index)
                           (xr:projection-view-image-right view) w
                           (xr:projection-view-image-bottom view) h
                           (xr:projection-view-index view) index
                           (xr:projection-view-swapchain view) swapchain)
                     ;; draw the frame for the specified view at the
                     ;; specified time
                     (draw-xr-frame/1 view at)
                     ;; then add it to the list of layers to display
                     (vector-push-extend view layers))))))))

(defvar *random* (make-random-state))
(defun draw-xr-frame/1 (view at)
  (declare (ignorable at))
  (when *fbo*
    (let (;; extract some vars we will use from view object
          (vx (xr:projection-view-image-left view))
          (vy (xr:projection-view-image-top view))
          (vw (xr:projection-view-image-right view))
          (vh (xr:projection-view-image-bottom view))
          (fov (xr:projection-view-fov view))
          (p (xr:projection-view-position view))
          (o (xr:projection-view-orientation view))
          (image (xr:projection-view-image view))
          ;; use the same random state for every call so that can
          ;; define our 'world'
          (*random-state* (make-random-state *random*)))
      ;; set up the fbo
      (gl:named-framebuffer-texture (car *fbo*) :color-attachment0 image 0)
      (gl:bind-framebuffer :framebuffer (car *fbo*))
      (gl:viewport vx vy vw vh)
      ;; clear the buffers
      (gl:clear-color 0.1 0.4 0.9 1)
      (gl:clear :color-buffer :depth-buffer)
      (gl:enable :depth-test)

;;; calculate the projection matrix for this view
      ;; old-style gl immediate mode, to keep things shorter :)
      (gl:with-pushed-matrix* (:projection)
        (gl:load-identity)
        (gl:mult-matrix (frustum (xr:angle-left fov) (xr:angle-right fov)
                                 (xr:angle-up fov) (xr:angle-down fov)
                                 ;; near plane should be fairly close,
                                 ;; so hands won't disappear when
                                 ;; close to eyes. 10cm seems
                                 ;; reasonable, maybe a bit closer
                                 0.1
                                 ;; and far plane should be decided
                                 ;; with usual concerns of far/near
                                 ;; ratio vs depth buffer precision vs
                                 ;; scene distance
                                 100.0))
;;; and modelview matrix
        (gl:with-pushed-matrix* (:modelview)
          (gl:load-identity)
          (gl:mult-matrix (inverse-pose-matrix p o))
          (flet ((q (x y w h &optional (z 0))
                   (gl:vertex (- x w) (- y h) z)
                   (gl:vertex (- x w) (+ y h) z)
                   (gl:vertex (+ x w) (+ y h) z)
                   (gl:vertex (+ x w) (- y h) z))
                 (fq (x z w h &optional (y 0))
                   (gl:vertex (- x w) y (- z h))
                   (gl:vertex (- x w) y (+ z h))
                   (gl:vertex (+ x w) y (+ z h))
                   (gl:vertex (+ x w) y (- z h))))
;;; draw some quads for the "world" (not too many, since we only have
;;; 8ms or so to draw both eyes, and immdiate mode isn't all that
;;; fast, especially through FFI
            (gl:with-primitive :quads
              (loop repeat 100
                    for w = (+ 0.01 (random 1.0))
                    do (gl:color (+ 0.1 (random 1.0))
                                 (+ 0.1 (Random 1.0))
                                 (+ 0.1 (random 1.0)) 1)
                       (q (- (random 20.0) 10)
                          (- (random 20.0) 10)
                          w w
                          (- (random 20.0) 10))
                       (fq (- (random 20.0) 10)
                           (- (random 20.0) 10)
                           w w
                           (- (random 20.0) 10))))

;;; Here is where we check the 'aim' action we attached to the left
;;; hand. We use xr:locate-space to ask where one space is relative to
;;; another. In this case, we get position and orientation of the
;;; `aim` space attached to left hand, relative to the world space.
            (let* ((pose (xr:locate-space *aim-space* *space* at))
                   (position (xr:pose-position pose))
                   (orientation (xr:pose-orientation pose)))
              ;; these might be NIL if the runtime doesn't know where
              ;; that input is (there are also
              ;; xr:pose-position-tracked and
              ;; xr:pose-orientation-tracked flags indicating the
              ;; input isn't currently tracked, so might be last-know
              ;; state or estimated)
              (when (and position orientation)
                (gl:with-pushed-matrix* (:modelview)
                  ;; transform into the aim coordinate system
                  (gl:mult-matrix (pose-matrix position orientation))
                  (gl:with-primitive :triangles
                    ;; and draw a triangle pointing towards the aim
                    ;; direction (- Z)
                    (gl:color 1 0 0 1)
                    (gl:vertex 0 0 -0.1)
                    (gl:color 1 1 1 1)
                    (gl:vertex -0.01 0 0)
                    (gl:vertex 0.01 0 0)
                    ;; 3b-openxr automatically queries velocity with
                    ;; locate-space, so draw those too if returned
                    (let ((v (xr:pose+velocity-velocity pose)))
                      (when v
                        (gl:color 0 0 1 1)
                        (gl:vertex (xr:x v) (xr:y v) (xr:z v))
                        (gl:vertex -0.01 0 0)
                        (gl:vertex 0.01 0 0)))
                    (let ((v (xr:pose+velocity-angular-velocity pose)))
                      (when v
                        (gl:color 0 1 0 1)
                        (gl:vertex (* 0.001 (xr:x v))
                                   (* 0.001 (xr:y v))
                                   (* 0.001 (xr:z v)))
                        (gl:vertex -0.01 0 0)
                        (gl:vertex 0.01 0 0)))))))

;;; we can query the bounds of a :stage space, so we will use that to
;;; draw a floor
            (when (eql *reference-type* :stage)
              (let* ((b (xr:get-reference-space-bounds-rect *session*
                                                            :stage)))
                ;; if bounds are unavailable, we get NIL here
                (when (vectorp b)
                  (let ((w (/ (xr:width b) 2))
                        (h (/ (xr:height b) 2))
                        (sw 0.01))
                    ;; draw transparent floor
                    (gl:disable :depth-test)
                    (gl:enable :blend)
                    (gl:blend-func :src-alpha :one-minus-src-alpha)
                    (gl:with-primitive :quads
                      (gl:color 1 1 1 0.3)
                      (fq 0 0 w h))
                    ;; with a grid slightly above it
                    (gl:enable :depth-test)
                    (gl:with-primitive :quads
                      (gl:color 0 0 1 1)
                      (loop for i from -5 to 5
                            do (fq (* w (/ i 5)) (/ sw -2)
                                   sw (+ h sw)
                                   0.01)
                               (fq (/ sw -2) (* h (/ i 5))
                                   (+ w sw) sw
                                   0.01)))))))
            ;; draw the right hand joints
            (when (and *hand* (eql *xr-state* :focused))
              (gl:enable :depth-test)
              (draw-hand *hand* at)))))
      (gl:bind-framebuffer :framebuffer 0))))

(defun draw-hand (hand at)
  (xr:with-debug-utils-label (*session* "hand")
;;; similar to the `aim` space, we query the hand joint positions
;;; relative to our world space
    (let ((loc (xr:locate-hand-joints-ext hand *space* at)))
      ;; which might return NIL if not available
      (when loc
        (flet ((tet (r)
                 (let* ((+x (* r (cos #.(float (* pi 30/180) 1.0))))
                        (-x (- +x))
                        (y (- (* r (sin #.(float (* pi 30/180) 1.0)))))
                        (z (* r -2)))
                   (gl:vertex  0 r 0)
                   (gl:vertex -x y 0)
                   (gl:vertex +x y 0)

                   (gl:vertex -x y 0)
                   (gl:vertex  0 0 z)
                   (gl:vertex +x y 0)

                   (gl:vertex  0 r 0)
                   (gl:vertex  0 0 z)
                   (gl:vertex -x y 0)

                   (gl:vertex  0 r 0)
                   (gl:vertex +x y 0)
                   (gl:vertex  0 0 z))))
          (gl:disable :texture-2d)
;;; it returns a vector of 26 joints, currently a plist of pose and
;;; radius (which should probably change at some point, and might be
;;; replaced by a non-consing (or at least less consing) api instead)
          (loop for l across loc
                for p = (getf l :pose)
                for r = (getf l :radius)
                when p
                  ;; the joints are all in the same space, not a
                  ;; hierarchy. Order is fixed so they can be used for
                  ;; skinning or to attach things to specific joints.
                  do (gl:with-pushed-matrix* (:modelview)
                       (gl:mult-matrix (pose-matrix (xr:pose-position p)
                                                    (xr:pose-orientation p)))
                       (gl:color 0.08 0.0 1 1)
                       (gl:with-primitive :triangles
                         (tet r)))))))))

(defun draw-desktop-frame ()
  (let ((now (get-internal-real-time)))
    (gl:viewport 0 0
                 (glop:window-width *window*)
                 (glop:window-height *window*))
    ;; change colors so we can tell when it gets stuck
    (flet ((c (w f)
             (+ w (* w (sin (* f (/  now internal-time-units-per-second)))))))
      (gl:clear-color (c 0.1 1) (c 0.08 0.5) (c 0.09 0.3) 1))
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    ;; draw something to indicate current xr state
    (destructuring-bind (x y r g b)
        (case *xr-state*
          (:idle '(0 1 1 1 1))
          (:ready '(1 1 1 0 1))
          (:synchronized '(2 1 0 0.25 0))
          (:visible '(3 1 0 0.5 0))
          (:focused '(4 1 0 1 0))
          (:stopping '(3 2 1 0.5 0.5))
          (:loss-pending '(2 2 1 0.2 0.1))
          (:exiting '(1 2 1 0 0)))
      (gl:with-pushed-matrix* (:modelview)
        (gl:load-identity)
        (gl:translate -1 -1 0)
        (gl:scale (/ 2 800) (/ 2 600) 1)
        (gl:with-primitives :quads
          (gl:color r g b 1)
          (gl:vertex (* x 32) (* y 32))
          (gl:vertex (* (1+ x) 32) (* y 32))
          (gl:vertex (* (1+ x) 32) (* (1+ y) 32))
          (gl:vertex (* x 32) (* (1+ y) 32)))))
    (glop:swap-buffers *window*)))

;;;; math routines used to calculate projection and pose matrices

(defun frustum (left right top bottom near far)
  (declare (type single-float left right top bottom near far))
  ;; l,r,t,b are angles, intersect with near to get clip planes for
  ;; frustum calc
  (let* ((left (* near (tan left)))
         (right (* near (tan right)))
         (top (* near (tan top)))
         (bottom (* near (tan bottom)))
         (a (/ (+ right left) (- right left)))
         (b (/ (+ top bottom) (- top bottom)))
         (c (- (/ (+ far near) (- far near))))
         (d (- (/ (* 2 far near) (- far near))))
         (e (/ (* 2 near) (- right left)))
         (f (/ (* 2 near) (- top bottom))))
    (sb-cga:matrix   e 0f0    a 0f0
                   0f0   f    b 0f0
                   0f0 0f0    c   d
                   0f0 0f0 -1f0 0f0)))

(defun quat-matrix (quat &optional (dir 1.0))
  (declare (type (simple-array single-float (4)))
           (type single-float dir))
  (let* ((a (aref quat 3))
         (b (* dir (aref quat 0)))
         (c (* dir (aref quat 1)))
         (d (* dir (aref quat 2)))
         (s (/ 2 (+ (* a a) (* b b) (* c c) (* d d))))
         (bs (* b s))
         (cs (* c s))
         (ds (* d s))
         (ab (* a bs))
         (ac (* a cs))
         (ad (* a ds))
         (bb (* b bs))
         (bc (* b cs))
         (bd (* b ds))
         (cc (* c cs))
         (cd (* c ds))
         (dd (* d ds)))
    (sb-cga:matrix (- 1 cc dd) (- bc ad) (+ bd ac) 0.0
                   (+ bc ad) (- 1 bb dd) (- cd ab) 0.0
                   (- bd ac) (+ cd ab) (- 1 bb cc) 0.0
                   0.0 0.0 0.0 1.0)))

(defun pose-matrix (position orientation)
  (cond
    ((and position orientation)
     (sb-cga:matrix* (sb-cga:translate position) (quat-matrix orientation)))
    (position (sb-cga:translate position))
    (t (sb-cga:identity-matrix))))

(defun inverse-pose-matrix (position orientation)
  (cond
    ((and position orientation)
     (sb-cga:matrix* (quat-matrix orientation -1.0)
                     (sb-cga:translate (sb-cga:vec* position -1.0))))
    (position (sb-cga:translate position))
    (t (sb-cga:identity-matrix))))


;;;; some extra stuff to make glop work

(defmethod glop:on-resize (window w h)
  (declare (ignore window))
  (gl:viewport 0 0 w h))

(defmethod glop:on-draw (window) (declare (ignore window)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window x y dx dy)))

(defmethod glop:on-close (window) (declare (ignore window)))

(defmethod glop:on-key (window pressed keycode keysym text)
  (when (and (not pressed) (eq keysym :escape))
    (glop:push-close-event window)))

(defmethod glop:on-button (window pressed button) (declare (ignore window)))


