(in-package #:3b-openxr-wrappers)

;;; 9. Session

;; 9.1. Session Lifecycle

;; 9.2. Session Creation


(defun create-session (system-id ;; session-create-info
                       ;; :gl-win32 :gl-xlib :gl-xcb :gl-wayland
                       ;; :d3d11 :d3d12 :vulkan
                       api
                       &key
                         ;; session-create-info
                         create-flags ;; no valid values currently
                         ;;todo: session-create-info-overlay-extx

                         ;; graphics-binding-opengl-win32
                         hdc hglrc

                         ;; graphics-binding-opengl-xlib
                         display visual-id
                         glx-fbconfig glx-drawable glx-context

                         ;; graphics-binding-opengl-xcb
                         connection screen-number
                         fb-config-id
                         ;; shared with -xlib
                         ;; visual-id glx-drawable glx-context

                         ;; graphics-binding-opengl-wayland-khr
                         ;; shared with -xlib display

                         ;; graphics-binding-d3d11-khr, -d3d12
                         device
                         ;; graphics-binding-d3d12-khr
                         queue

                         ;; todo: graphics-binding-opengl-esandroid-khr

                         ;; graphics-binding-vulkan-khr
                         instance physical-device
                         ;; device shared with d3d
                         queue-family-index queue-index)
  (declare (ignorable display visual-id glx-fbconfig glx-drawable glx-context
                      connection screen-number fb-config-id
                      device queue instance physical-device
                      queue-family-index queue-index))
  (cffi:with-foreign-object (sci '(:struct %:session-create-info))
    (cffi:with-foreign-slots ((%:type
                               %:next
                               %:create-flags
                               %:system-id)
                              sci (:struct %:session-create-info))
      (setf %:type :type-session-create-info
            %:next (cffi:null-pointer)
            %:create-flags create-flags
            %:system-id system-id)
      (ecase api
        (:gl-win32
         (cffi:with-foreign-object (gb '(:struct %:graphics-binding-opengl-win32-khr))
           (cffi:with-foreign-slots ((%:type
                                      (next2 %:next)
                                      %:hdc
                                      %:hglrc)
                                     gb (:struct %:graphics-binding-opengl-win32-khr))
             (setf %:type :type-graphics-binding-opengl-win32-khr
                   %:next gb                 ;; parent struct
                   next2 (cffi:null-pointer) ;; gb
                   %:hdc hdc
                   %:hglrc hglrc)
             (cffi:with-foreign-object (s '%:session)
               (check-result (%:create-session *instance* sci s))
               (cffi:mem-ref s '%:session)))))
        (:gl-xlib (error "todo"))
        (:gl-xcb(error "todo"))
        (:gl-wayland(error "todo"))
        (:d3d11(error "todo"))
        (:d3d12(error "todo"))
        (:vulkan(error "todo"))))))

(defmacro with-session ((session system-id &rest r) &body body)
  `(let ((,session (create-session ,system-id ,@r)))
     (format t "~&created session #x~x~%" ,session)
     (unwind-protect
          (progn ,@body)
       (format t "destroy session #x~x~%" ,session)
       (%:destroy-session ,session))))

;; 9.3. Session Control

(defun begin-session (session view-configuration-type)
  (with-session-begin-info (sbi
                            :primary-view-configuration-type
                            view-configuration-type)
    (check-result (%:begin-session session sbi))))

(import-export %:end-session %:request-exit-session)

;; 9.4. Session States
