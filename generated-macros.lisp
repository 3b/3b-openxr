(in-package #:3b-openxr-mid-level)
;; used by with-two-call
(defparameter %struct-types% (make-hash-table))

(defmacro with-vector-2f ((pointer &key %slots (x nil x-p) (y nil y-p))
                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vector-2f))
     (cffi:with-foreign-slots ((%:x %:y)
                               ,pointer (:struct %:vector-2f))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-vector-3f ((pointer &key %slots (x nil x-p) (y nil y-p)
                                     (z nil z-p))
                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vector-3f))
     (cffi:with-foreign-slots ((%:x %:y %:z)
                               ,pointer (:struct %:vector-3f))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y))
             ,@(when z-p `(%:z ,z)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-vector-4f ((pointer &key %slots (x nil x-p) (y nil y-p)
                                     (z nil z-p) (w nil w-p))
                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vector-4f))
     (cffi:with-foreign-slots ((%:x %:y %:z %:w)
                               ,pointer (:struct %:vector-4f))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y))
             ,@(when z-p `(%:z ,z))
             ,@(when w-p `(%:w ,w)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-color-4f ((pointer &key %slots (r nil r-p) (g nil g-p)
                                    (b nil b-p) (a nil a-p))
                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:color-4f))
     (cffi:with-foreign-slots ((%:r %:g %:b %:a)
                               ,pointer (:struct %:color-4f))
       (setf ,@(when r-p `(%:r ,r))
             ,@(when g-p `(%:g ,g))
             ,@(when b-p `(%:b ,b))
             ,@(when a-p `(%:a ,a)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-quaternion-f ((pointer &key %slots (x nil x-p) (y nil y-p)
                                        (z nil z-p) (w nil w-p))
                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:quaternion-f))
     (cffi:with-foreign-slots ((%:x %:y %:z %:w)
                               ,pointer (:struct %:quaternion-f))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y))
             ,@(when z-p `(%:z ,z))
             ,@(when w-p `(%:w ,w)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-pose-f ((pointer &key %slots (orientation nil orientation-p)
                                  (position nil position-p))
                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:pose-f))
     (cffi:with-foreign-slots ((%:orientation %:position)
                               ,pointer (:struct %:pose-f))
       (setf ,@(when orientation-p `(%:orientation ,orientation))
             ,@(when position-p `(%:position ,position)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-offset-2d-f ((pointer &key %slots (x nil x-p) (y nil y-p))
                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:offset-2d-f))
     (cffi:with-foreign-slots ((%:x %:y)
                               ,pointer (:struct %:offset-2d-f))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-extent-2d-f ((pointer &key %slots (width nil width-p)
                                       (height nil height-p))
                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:extent-2d-f))
     (cffi:with-foreign-slots ((%:width %:height)
                               ,pointer (:struct %:extent-2d-f))
       (setf ,@(when width-p `(%:width ,width))
             ,@(when height-p `(%:height ,height)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-rect-2d-f ((pointer &key %slots (offset nil offset-p)
                                     (extent nil extent-p))
                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:rect-2d-f))
     (cffi:with-foreign-slots ((%:offset %:extent)
                               ,pointer (:struct %:rect-2d-f))
       (setf ,@(when offset-p `(%:offset ,offset))
             ,@(when extent-p `(%:extent ,extent)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-offset-2d-i ((pointer &key %slots (x nil x-p) (y nil y-p))
                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:offset-2d-i))
     (cffi:with-foreign-slots ((%:x %:y)
                               ,pointer (:struct %:offset-2d-i))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-extent-2d-i ((pointer &key %slots (width nil width-p)
                                       (height nil height-p))
                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:extent-2d-i))
     (cffi:with-foreign-slots ((%:width %:height)
                               ,pointer (:struct %:extent-2d-i))
       (setf ,@(when width-p `(%:width ,width))
             ,@(when height-p `(%:height ,height)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-rect-2d-i ((pointer &key %slots (offset nil offset-p)
                                     (extent nil extent-p))
                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:rect-2d-i))
     (cffi:with-foreign-slots ((%:offset %:extent)
                               ,pointer (:struct %:rect-2d-i))
       (setf ,@(when offset-p `(%:offset ,offset))
             ,@(when extent-p `(%:extent ,extent)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-api-layer-properties ((pointer &key %slots
                                                (next '(cffi:null-pointer))
                                                (layer-name nil layer-name-p)
                                                (spec-version nil
                                                 spec-version-p)
                                                (layer-version nil
                                                 layer-version-p)
                                                (description nil description-p))
                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:api-layer-properties))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-name
                                %:spec-version
                                %:layer-version
                                %:description)
                               ,pointer (:struct %:api-layer-properties))
       (setf %:type :type-api-layer-properties
             %:next ,next
             ,@(when spec-version-p `(%:spec-version ,spec-version))
             ,@(when layer-version-p `(%:layer-version ,layer-version)))
       (if (and ,layer-name-p (not ,layer-name))
           (setf %:layer-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,layer-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:api-layer-properties) '%:layer-name)
            %:+max-api-layer-name-size+ :encoding :utf-8))
       (if (and ,description-p (not ,description))
           (setf %:description (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,description "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:api-layer-properties) '%:description)
            %:+max-api-layer-description-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:api-layer-properties %struct-types%)
      ':type-api-layer-properties)

(defmacro with-extension-properties ((pointer &key %slots
                                                (next '(cffi:null-pointer))
                                                (extension-name nil
                                                 extension-name-p)
                                                (extension-version nil
                                                 extension-version-p))
                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:extension-properties))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:extension-name
                                %:extension-version)
                               ,pointer (:struct %:extension-properties))
       (setf %:type :type-extension-properties
             %:next ,next
             ,@(when extension-version-p `(%:extension-version ,extension-version)))
       (if (and ,extension-name-p (not ,extension-name))
           (setf %:extension-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,extension-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:extension-properties) '%:extension-name)
            %:+max-extension-name-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:extension-properties %struct-types%)
      ':type-extension-properties)

(defmacro with-application-info ((pointer &key %slots
                                            (application-name nil
                                             application-name-p)
                                            (application-version nil
                                             application-version-p)
                                            (engine-name nil engine-name-p)
                                            (engine-version nil
                                             engine-version-p)
                                            (api-version nil api-version-p))
                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:application-info))
     (cffi:with-foreign-slots ((%:application-name
                                %:application-version
                                %:engine-name
                                %:engine-version
                                %:api-version)
                               ,pointer (:struct %:application-info))
       (setf ,@(when application-version-p `(%:application-version ,application-version))
             ,@(when engine-version-p `(%:engine-version ,engine-version))
             ,@(when api-version-p `(%:api-version ,api-version)))
       (if (and ,application-name-p (not ,application-name))
           (setf %:application-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,application-name "")
            (cffi:foreign-slot-pointer ,pointer '(:struct %:application-info)
             '%:application-name)
            %:+max-application-name-size+ :encoding :utf-8))
       (if (and ,engine-name-p (not ,engine-name))
           (setf %:engine-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,engine-name "")
            (cffi:foreign-slot-pointer ,pointer '(:struct %:application-info)
             '%:engine-name)
            %:+max-engine-name-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-instance-create-info ((pointer &key %slots
                                                (next '(cffi:null-pointer))
                                                (create-flags nil
                                                 create-flags-p)
                                                (application-info nil
                                                 application-info-p)
                                                (enabled-api-layer-count nil
                                                 enabled-api-layer-count-p)
                                                (enabled-api-layer-names nil
                                                 enabled-api-layer-names-p)
                                                (enabled-extension-count nil
                                                 enabled-extension-count-p)
                                                (enabled-extension-names nil
                                                 enabled-extension-names-p))
                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:instance-create-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:create-flags
                                %:application-info
                                %:enabled-api-layer-count
                                %:enabled-api-layer-names
                                %:enabled-extension-count
                                %:enabled-extension-names)
                               ,pointer (:struct %:instance-create-info))
       (setf %:type :type-instance-create-info
             %:next ,next
             ,@(when create-flags-p `(%:create-flags ,create-flags))
             ,@(when application-info-p `(%:application-info ,application-info))
             ,@(when enabled-api-layer-count-p `(%:enabled-api-layer-count ,enabled-api-layer-count))
             ,@(when enabled-api-layer-names-p `(%:enabled-api-layer-names ,enabled-api-layer-names))
             ,@(when enabled-extension-count-p `(%:enabled-extension-count ,enabled-extension-count))
             ,@(when enabled-extension-names-p `(%:enabled-extension-names ,enabled-extension-names)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:instance-create-info %struct-types%)
      ':type-instance-create-info)

(defmacro with-instance-properties ((pointer &key %slots
                                               (next '(cffi:null-pointer))
                                               (runtime-version nil
                                                runtime-version-p)
                                               (runtime-name nil
                                                runtime-name-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:instance-properties))
     (cffi:with-foreign-slots ((%:type %:next %:runtime-version %:runtime-name)
                               ,pointer (:struct %:instance-properties))
       (setf %:type :type-instance-properties
             %:next ,next
             ,@(when runtime-version-p `(%:runtime-version ,runtime-version)))
       (if (and ,runtime-name-p (not ,runtime-name))
           (setf %:runtime-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,runtime-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:instance-properties) '%:runtime-name)
            %:+max-runtime-name-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:instance-properties %struct-types%)
      ':type-instance-properties)

(defmacro with-system-get-info ((pointer &key %slots
                                           (next '(cffi:null-pointer))
                                           (form-factor nil form-factor-p))
                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-get-info))
     (cffi:with-foreign-slots ((%:type %:next %:form-factor)
                               ,pointer (:struct %:system-get-info))
       (setf %:type :type-system-get-info
             %:next ,next
             ,@(when form-factor-p `(%:form-factor ,form-factor)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-get-info %struct-types%)
      ':type-system-get-info)

(defmacro with-system-properties ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (system-id nil system-id-p)
                                             (vendor-id nil vendor-id-p)
                                             (system-name nil system-name-p)
                                             (graphics-properties nil
                                              graphics-properties-p)
                                             (tracking-properties nil
                                              tracking-properties-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-properties))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:system-id
                                %:vendor-id
                                %:system-name
                                %:graphics-properties
                                %:tracking-properties)
                               ,pointer (:struct %:system-properties))
       (setf %:type :type-system-properties
             %:next ,next
             ,@(when system-id-p `(%:system-id ,system-id))
             ,@(when vendor-id-p `(%:vendor-id ,vendor-id))
             ,@(when graphics-properties-p `(%:graphics-properties ,graphics-properties))
             ,@(when tracking-properties-p `(%:tracking-properties ,tracking-properties)))
       (if (and ,system-name-p (not ,system-name))
           (setf %:system-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,system-name "")
            (cffi:foreign-slot-pointer ,pointer '(:struct %:system-properties)
             '%:system-name)
            %:+max-system-name-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-properties %struct-types%)
      ':type-system-properties)

(defmacro with-system-graphics-properties ((pointer &key %slots
                                                      (max-swapchain-image-height
                                                       nil
                                                       max-swapchain-image-height-p)
                                                      (max-swapchain-image-width
                                                       nil
                                                       max-swapchain-image-width-p)
                                                      (max-layer-count nil
                                                       max-layer-count-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-graphics-properties))
     (cffi:with-foreign-slots ((%:max-swapchain-image-height
                                %:max-swapchain-image-width
                                %:max-layer-count)
                               ,pointer (:struct %:system-graphics-properties))
       (setf ,@(when max-swapchain-image-height-p `(%:max-swapchain-image-height ,max-swapchain-image-height))
             ,@(when max-swapchain-image-width-p `(%:max-swapchain-image-width ,max-swapchain-image-width))
             ,@(when max-layer-count-p `(%:max-layer-count ,max-layer-count)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-system-tracking-properties ((pointer &key %slots
                                                      (orientation-tracking nil
                                                       orientation-tracking-p)
                                                      (position-tracking nil
                                                       position-tracking-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-tracking-properties))
     (cffi:with-foreign-slots ((%:orientation-tracking %:position-tracking)
                               ,pointer (:struct %:system-tracking-properties))
       (setf ,@(when orientation-tracking-p `(%:orientation-tracking ,orientation-tracking))
             ,@(when position-tracking-p `(%:position-tracking ,position-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-graphics-binding-opengl-win32-khr ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (hdc nil hdc-p)
                                                             (hglrc nil
                                                              hglrc-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-opengl-win32-khr))
     (cffi:with-foreign-slots ((%:type %:next %:hdc %:hglrc)
                               ,pointer (:struct %:graphics-binding-opengl-win32-khr))
       (setf %:type :type-graphics-binding-opengl-win32-khr
             %:next ,next
             ,@(when hdc-p `(%:hdc ,hdc))
             ,@(when hglrc-p `(%:hglrc ,hglrc)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-opengl-win32-khr %struct-types%)
      ':type-graphics-binding-opengl-win32-khr)

(defmacro with-graphics-binding-opengl-xlib-khr ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (x-display nil
                                                             x-display-p)
                                                            (visualid nil
                                                             visualid-p)
                                                            (glx-fbconfig nil
                                                             glx-fbconfig-p)
                                                            (glx-drawable nil
                                                             glx-drawable-p)
                                                            (glx-context nil
                                                             glx-context-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-opengl-xlib-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:x-display
                                %:visualid
                                %:glx-fbconfig
                                %:glx-drawable
                                %:glx-context)
                               ,pointer (:struct %:graphics-binding-opengl-xlib-khr))
       (setf %:type :type-graphics-binding-opengl-xlib-khr
             %:next ,next
             ,@(when x-display-p `(%:x-display ,x-display))
             ,@(when visualid-p `(%:visualid ,visualid))
             ,@(when glx-fbconfig-p `(%:glx-fbconfig ,glx-fbconfig))
             ,@(when glx-drawable-p `(%:glx-drawable ,glx-drawable))
             ,@(when glx-context-p `(%:glx-context ,glx-context)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-opengl-xlib-khr %struct-types%)
      ':type-graphics-binding-opengl-xlib-khr)

(defmacro with-graphics-binding-opengl-xcb-khr ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (connection nil
                                                            connection-p)
                                                           (screen-number nil
                                                            screen-number-p)
                                                           (fbconfigid nil
                                                            fbconfigid-p)
                                                           (visualid nil
                                                            visualid-p)
                                                           (glx-drawable nil
                                                            glx-drawable-p)
                                                           (glx-context nil
                                                            glx-context-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-opengl-xcb-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:connection
                                %:screen-number
                                %:fbconfigid
                                %:visualid
                                %:glx-drawable
                                %:glx-context)
                               ,pointer (:struct %:graphics-binding-opengl-xcb-khr))
       (setf %:type :type-graphics-binding-opengl-xcb-khr
             %:next ,next
             ,@(when connection-p `(%:connection ,connection))
             ,@(when screen-number-p `(%:screen-number ,screen-number))
             ,@(when fbconfigid-p `(%:fbconfigid ,fbconfigid))
             ,@(when visualid-p `(%:visualid ,visualid))
             ,@(when glx-drawable-p `(%:glx-drawable ,glx-drawable))
             ,@(when glx-context-p `(%:glx-context ,glx-context)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-opengl-xcb-khr %struct-types%)
      ':type-graphics-binding-opengl-xcb-khr)

(defmacro with-graphics-binding-opengl-wayland-khr ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (display nil
                                                                display-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-opengl-wayland-khr))
     (cffi:with-foreign-slots ((%:type %:next %:display)
                               ,pointer (:struct %:graphics-binding-opengl-wayland-khr))
       (setf %:type :type-graphics-binding-opengl-wayland-khr
             %:next ,next
             ,@(when display-p `(%:display ,display)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-opengl-wayland-khr %struct-types%)
      ':type-graphics-binding-opengl-wayland-khr)

(defmacro with-graphics-binding-d3d11-khr ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (device nil device-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-d3d11-khr))
     (cffi:with-foreign-slots ((%:type %:next %:device)
                               ,pointer (:struct %:graphics-binding-d3d11-khr))
       (setf %:type :type-graphics-binding-d3d11-khr
             %:next ,next
             ,@(when device-p `(%:device ,device)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-d3d11-khr %struct-types%)
      ':type-graphics-binding-d3d11-khr)

(defmacro with-graphics-binding-d3d12-khr ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (device nil device-p)
                                                      (queue nil queue-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-d3d12-khr))
     (cffi:with-foreign-slots ((%:type %:next %:device %:queue)
                               ,pointer (:struct %:graphics-binding-d3d12-khr))
       (setf %:type :type-graphics-binding-d3d12-khr
             %:next ,next
             ,@(when device-p `(%:device ,device))
             ,@(when queue-p `(%:queue ,queue)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-d3d12-khr %struct-types%)
      ':type-graphics-binding-d3d12-khr)

(defmacro with-graphics-binding-opengl-es-android-khr ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (display nil
                                                                   display-p)
                                                                  (config nil
                                                                   config-p)
                                                                  (context nil
                                                                   context-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-opengl-es-android-khr))
     (cffi:with-foreign-slots ((%:type %:next %:display %:config %:context)
                               ,pointer (:struct %:graphics-binding-opengl-es-android-khr))
       (setf %:type :type-graphics-binding-opengl-es-android-khr
             %:next ,next
             ,@(when display-p `(%:display ,display))
             ,@(when config-p `(%:config ,config))
             ,@(when context-p `(%:context ,context)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-opengl-es-android-khr %struct-types%)
      ':type-graphics-binding-opengl-es-android-khr)

(defmacro with-graphics-binding-vulkan-khr ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (instance nil
                                                        instance-p)
                                                       (physical-device nil
                                                        physical-device-p)
                                                       (device nil device-p)
                                                       (queue-family-index nil
                                                        queue-family-index-p)
                                                       (queue-index nil
                                                        queue-index-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-vulkan-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:instance
                                %:physical-device
                                %:device
                                %:queue-family-index
                                %:queue-index)
                               ,pointer (:struct %:graphics-binding-vulkan-khr))
       (setf %:type :type-graphics-binding-vulkan-khr
             %:next ,next
             ,@(when instance-p `(%:instance ,instance))
             ,@(when physical-device-p `(%:physical-device ,physical-device))
             ,@(when device-p `(%:device ,device))
             ,@(when queue-family-index-p `(%:queue-family-index ,queue-family-index))
             ,@(when queue-index-p `(%:queue-index ,queue-index)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-vulkan-khr %struct-types%)
      ':type-graphics-binding-vulkan-khr)

(defmacro with-session-create-info ((pointer &key %slots
                                               (next '(cffi:null-pointer))
                                               (create-flags nil
                                                create-flags-p)
                                               (system-id nil system-id-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:session-create-info))
     (cffi:with-foreign-slots ((%:type %:next %:create-flags %:system-id)
                               ,pointer (:struct %:session-create-info))
       (setf %:type :type-session-create-info
             %:next ,next
             ,@(when create-flags-p `(%:create-flags ,create-flags))
             ,@(when system-id-p `(%:system-id ,system-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:session-create-info %struct-types%)
      ':type-session-create-info)

(defmacro with-session-begin-info ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (primary-view-configuration-type
                                               nil
                                               primary-view-configuration-type-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:session-begin-info))
     (cffi:with-foreign-slots ((%:type %:next %:primary-view-configuration-type)
                               ,pointer (:struct %:session-begin-info))
       (setf %:type :type-session-begin-info
             %:next ,next
             ,@(when primary-view-configuration-type-p `(%:primary-view-configuration-type ,primary-view-configuration-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:session-begin-info %struct-types%)
      ':type-session-begin-info)

(defmacro with-swapchain-create-info ((pointer &key %slots
                                                 (next '(cffi:null-pointer))
                                                 (create-flags nil
                                                  create-flags-p)
                                                 (usage-flags nil
                                                  usage-flags-p)
                                                 (format nil format-p)
                                                 (sample-count nil
                                                  sample-count-p)
                                                 (width nil width-p)
                                                 (height nil height-p)
                                                 (face-count nil face-count-p)
                                                 (array-size nil array-size-p)
                                                 (mip-count nil mip-count-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-create-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:create-flags
                                %:usage-flags
                                %:format
                                %:sample-count
                                %:width
                                %:height
                                %:face-count
                                %:array-size
                                %:mip-count)
                               ,pointer (:struct %:swapchain-create-info))
       (setf %:type :type-swapchain-create-info
             %:next ,next
             ,@(when create-flags-p `(%:create-flags ,create-flags))
             ,@(when usage-flags-p `(%:usage-flags ,usage-flags))
             ,@(when format-p `(%:format ,format))
             ,@(when sample-count-p `(%:sample-count ,sample-count))
             ,@(when width-p `(%:width ,width))
             ,@(when height-p `(%:height ,height))
             ,@(when face-count-p `(%:face-count ,face-count))
             ,@(when array-size-p `(%:array-size ,array-size))
             ,@(when mip-count-p `(%:mip-count ,mip-count)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-create-info %struct-types%)
      ':type-swapchain-create-info)

(defmacro with-swapchain-image-opengl-khr ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (image nil image-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-opengl-khr))
     (cffi:with-foreign-slots ((%:type %:next %:image)
                               ,pointer (:struct %:swapchain-image-opengl-khr))
       (setf %:type :type-swapchain-image-opengl-khr
             %:next ,next
             ,@(when image-p `(%:image ,image)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-opengl-khr %struct-types%)
      ':type-swapchain-image-opengl-khr)

(defmacro with-swapchain-image-opengl-es-khr ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (image nil image-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-opengl-es-khr))
     (cffi:with-foreign-slots ((%:type %:next %:image)
                               ,pointer (:struct %:swapchain-image-opengl-es-khr))
       (setf %:type :type-swapchain-image-opengl-es-khr
             %:next ,next
             ,@(when image-p `(%:image ,image)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-opengl-es-khr %struct-types%)
      ':type-swapchain-image-opengl-es-khr)

(defmacro with-swapchain-image-vulkan-khr ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (image nil image-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-vulkan-khr))
     (cffi:with-foreign-slots ((%:type %:next %:image)
                               ,pointer (:struct %:swapchain-image-vulkan-khr))
       (setf %:type :type-swapchain-image-vulkan-khr
             %:next ,next
             ,@(when image-p `(%:image ,image)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-vulkan-khr %struct-types%)
      ':type-swapchain-image-vulkan-khr)

(defmacro with-swapchain-image-d3d11-khr ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (texture nil texture-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-d3d11-khr))
     (cffi:with-foreign-slots ((%:type %:next %:texture)
                               ,pointer (:struct %:swapchain-image-d3d11-khr))
       (setf %:type :type-swapchain-image-d3d11-khr
             %:next ,next
             ,@(when texture-p `(%:texture ,texture)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-d3d11-khr %struct-types%)
      ':type-swapchain-image-d3d11-khr)

(defmacro with-swapchain-image-d3d12-khr ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (texture nil texture-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-d3d12-khr))
     (cffi:with-foreign-slots ((%:type %:next %:texture)
                               ,pointer (:struct %:swapchain-image-d3d12-khr))
       (setf %:type :type-swapchain-image-d3d12-khr
             %:next ,next
             ,@(when texture-p `(%:texture ,texture)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-d3d12-khr %struct-types%)
      ':type-swapchain-image-d3d12-khr)

(defmacro with-swapchain-image-acquire-info ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer)))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-acquire-info))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:swapchain-image-acquire-info))
       (setf %:type :type-swapchain-image-acquire-info
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-acquire-info %struct-types%)
      ':type-swapchain-image-acquire-info)

(defmacro with-swapchain-image-wait-info ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (timeout nil timeout-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-wait-info))
     (cffi:with-foreign-slots ((%:type %:next %:timeout)
                               ,pointer (:struct %:swapchain-image-wait-info))
       (setf %:type :type-swapchain-image-wait-info
             %:next ,next
             ,@(when timeout-p `(%:timeout ,timeout)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-wait-info %struct-types%)
      ':type-swapchain-image-wait-info)

(defmacro with-swapchain-image-release-info ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer)))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-release-info))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:swapchain-image-release-info))
       (setf %:type :type-swapchain-image-release-info
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-release-info %struct-types%)
      ':type-swapchain-image-release-info)

(defmacro with-reference-space-create-info ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (reference-space-type
                                                        nil
                                                        reference-space-type-p)
                                                       (pose-in-reference-space
                                                        nil
                                                        pose-in-reference-space-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:reference-space-create-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:reference-space-type
                                %:pose-in-reference-space)
                               ,pointer (:struct %:reference-space-create-info))
       (setf %:type :type-reference-space-create-info
             %:next ,next
             ,@(when reference-space-type-p `(%:reference-space-type ,reference-space-type))
             ,@(when pose-in-reference-space-p `(%:pose-in-reference-space ,pose-in-reference-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:reference-space-create-info %struct-types%)
      ':type-reference-space-create-info)

(defmacro with-action-space-create-info ((pointer &key %slots
                                                    (next '(cffi:null-pointer))
                                                    (action nil action-p)
                                                    (subaction-path
                                                     %:+null-path+)
                                                    (pose-in-action-space nil
                                                     pose-in-action-space-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-space-create-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:action
                                %:subaction-path
                                %:pose-in-action-space)
                               ,pointer (:struct %:action-space-create-info))
       (setf %:type :type-action-space-create-info
             %:next ,next
             ,@(when action-p `(%:action ,action))
             %:subaction-path ,subaction-path
             ,@(when pose-in-action-space-p `(%:pose-in-action-space ,pose-in-action-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-space-create-info %struct-types%)
      ':type-action-space-create-info)

(defmacro with-space-location ((pointer &key %slots (next '(cffi:null-pointer))
                                          (location-flags nil location-flags-p)
                                          (pose nil pose-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-location))
     (cffi:with-foreign-slots ((%:type %:next %:location-flags %:pose)
                               ,pointer (:struct %:space-location))
       (setf %:type :type-space-location
             %:next ,next
             ,@(when location-flags-p `(%:location-flags ,location-flags))
             ,@(when pose-p `(%:pose ,pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-location %struct-types%)
      ':type-space-location)

(defmacro with-space-velocity ((pointer &key %slots (next '(cffi:null-pointer))
                                          (velocity-flags nil velocity-flags-p)
                                          (linear-velocity nil
                                           linear-velocity-p)
                                          (angular-velocity nil
                                           angular-velocity-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-velocity))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:velocity-flags
                                %:linear-velocity
                                %:angular-velocity)
                               ,pointer (:struct %:space-velocity))
       (setf %:type :type-space-velocity
             %:next ,next
             ,@(when velocity-flags-p `(%:velocity-flags ,velocity-flags))
             ,@(when linear-velocity-p `(%:linear-velocity ,linear-velocity))
             ,@(when angular-velocity-p `(%:angular-velocity ,angular-velocity)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-velocity %struct-types%)
      ':type-space-velocity)

(defmacro with-fov-f ((pointer &key %slots (angle-left nil angle-left-p)
                                 (angle-right nil angle-right-p)
                                 (angle-up nil angle-up-p)
                                 (angle-down nil angle-down-p))
                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:fov-f))
     (cffi:with-foreign-slots ((%:angle-left
                                %:angle-right
                                %:angle-up
                                %:angle-down)
                               ,pointer (:struct %:fov-f))
       (setf ,@(when angle-left-p `(%:angle-left ,angle-left))
             ,@(when angle-right-p `(%:angle-right ,angle-right))
             ,@(when angle-up-p `(%:angle-up ,angle-up))
             ,@(when angle-down-p `(%:angle-down ,angle-down)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-view ((pointer &key %slots (next '(cffi:null-pointer))
                                (pose nil pose-p) (fov nil fov-p))
                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view))
     (cffi:with-foreign-slots ((%:type %:next %:pose %:fov)
                               ,pointer (:struct %:view))
       (setf %:type :type-view
             %:next ,next
             ,@(when pose-p `(%:pose ,pose))
             ,@(when fov-p `(%:fov ,fov)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view %struct-types%)
      ':type-view)

(defmacro with-view-locate-info ((pointer &key %slots
                                            (next '(cffi:null-pointer))
                                            (view-configuration-type nil
                                             view-configuration-type-p)
                                            (display-time nil display-time-p)
                                            (space nil space-p))
                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-locate-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-type
                                %:display-time
                                %:space)
                               ,pointer (:struct %:view-locate-info))
       (setf %:type :type-view-locate-info
             %:next ,next
             ,@(when view-configuration-type-p `(%:view-configuration-type ,view-configuration-type))
             ,@(when display-time-p `(%:display-time ,display-time))
             ,@(when space-p `(%:space ,space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-locate-info %struct-types%)
      ':type-view-locate-info)

(defmacro with-view-state ((pointer &key %slots (next '(cffi:null-pointer))
                                      (view-state-flags nil view-state-flags-p))
                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-state))
     (cffi:with-foreign-slots ((%:type %:next %:view-state-flags)
                               ,pointer (:struct %:view-state))
       (setf %:type :type-view-state
             %:next ,next
             ,@(when view-state-flags-p `(%:view-state-flags ,view-state-flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-state %struct-types%)
      ':type-view-state)

(defmacro with-view-configuration-view ((pointer &key %slots
                                                   (next '(cffi:null-pointer))
                                                   (recommended-image-rect-width
                                                    nil
                                                    recommended-image-rect-width-p)
                                                   (max-image-rect-width nil
                                                    max-image-rect-width-p)
                                                   (recommended-image-rect-height
                                                    nil
                                                    recommended-image-rect-height-p)
                                                   (max-image-rect-height nil
                                                    max-image-rect-height-p)
                                                   (recommended-swapchain-sample-count
                                                    nil
                                                    recommended-swapchain-sample-count-p)
                                                   (max-swapchain-sample-count
                                                    nil
                                                    max-swapchain-sample-count-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-configuration-view))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:recommended-image-rect-width
                                %:max-image-rect-width
                                %:recommended-image-rect-height
                                %:max-image-rect-height
                                %:recommended-swapchain-sample-count
                                %:max-swapchain-sample-count)
                               ,pointer (:struct %:view-configuration-view))
       (setf %:type :type-view-configuration-view
             %:next ,next
             ,@(when recommended-image-rect-width-p `(%:recommended-image-rect-width ,recommended-image-rect-width))
             ,@(when max-image-rect-width-p `(%:max-image-rect-width ,max-image-rect-width))
             ,@(when recommended-image-rect-height-p `(%:recommended-image-rect-height ,recommended-image-rect-height))
             ,@(when max-image-rect-height-p `(%:max-image-rect-height ,max-image-rect-height))
             ,@(when recommended-swapchain-sample-count-p `(%:recommended-swapchain-sample-count ,recommended-swapchain-sample-count))
             ,@(when max-swapchain-sample-count-p `(%:max-swapchain-sample-count ,max-swapchain-sample-count)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-configuration-view %struct-types%)
      ':type-view-configuration-view)

(defmacro with-swapchain-sub-image ((pointer &key %slots
                                               (swapchain nil swapchain-p)
                                               (image-rect nil image-rect-p)
                                               (image-array-index nil
                                                image-array-index-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-sub-image))
     (cffi:with-foreign-slots ((%:swapchain %:image-rect %:image-array-index)
                               ,pointer (:struct %:swapchain-sub-image))
       (setf ,@(when swapchain-p `(%:swapchain ,swapchain))
             ,@(when image-rect-p `(%:image-rect ,image-rect))
             ,@(when image-array-index-p `(%:image-array-index ,image-array-index)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-composition-layer-projection-view ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (pose nil pose-p)
                                                             (fov nil fov-p)
                                                             (sub-image nil
                                                              sub-image-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-projection-view))
     (cffi:with-foreign-slots ((%:type %:next %:pose %:fov %:sub-image)
                               ,pointer (:struct %:composition-layer-projection-view))
       (setf %:type :type-composition-layer-projection-view
             %:next ,next
             ,@(when pose-p `(%:pose ,pose))
             ,@(when fov-p `(%:fov ,fov))
             ,@(when sub-image-p `(%:sub-image ,sub-image)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-projection-view %struct-types%)
      ':type-composition-layer-projection-view)

(defmacro with-composition-layer-projection ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer))
                                                        (layer-flags nil
                                                         layer-flags-p)
                                                        (space nil space-p)
                                                        (view-count nil
                                                         view-count-p)
                                                        (views nil views-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-projection))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:view-count
                                %:views)
                               ,pointer (:struct %:composition-layer-projection))
       (setf %:type :type-composition-layer-projection
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when view-count-p `(%:view-count ,view-count))
             ,@(when views-p `(%:views ,views)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-projection %struct-types%)
      ':type-composition-layer-projection)

(defmacro with-composition-layer-quad ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (layer-flags nil
                                                   layer-flags-p)
                                                  (space nil space-p)
                                                  (eye-visibility nil
                                                   eye-visibility-p)
                                                  (sub-image nil sub-image-p)
                                                  (pose nil pose-p)
                                                  (size nil size-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-quad))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:eye-visibility
                                %:sub-image
                                %:pose
                                %:size)
                               ,pointer (:struct %:composition-layer-quad))
       (setf %:type :type-composition-layer-quad
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when eye-visibility-p `(%:eye-visibility ,eye-visibility))
             ,@(when sub-image-p `(%:sub-image ,sub-image))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when size-p `(%:size ,size)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-quad %struct-types%)
      ':type-composition-layer-quad)

(defmacro with-composition-layer-cylinder-khr ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (layer-flags nil
                                                           layer-flags-p)
                                                          (space nil space-p)
                                                          (eye-visibility nil
                                                           eye-visibility-p)
                                                          (sub-image nil
                                                           sub-image-p)
                                                          (pose nil pose-p)
                                                          (radius nil radius-p)
                                                          (central-angle nil
                                                           central-angle-p)
                                                          (aspect-ratio nil
                                                           aspect-ratio-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-cylinder-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:eye-visibility
                                %:sub-image
                                %:pose
                                %:radius
                                %:central-angle
                                %:aspect-ratio)
                               ,pointer (:struct %:composition-layer-cylinder-khr))
       (setf %:type :type-composition-layer-cylinder-khr
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when eye-visibility-p `(%:eye-visibility ,eye-visibility))
             ,@(when sub-image-p `(%:sub-image ,sub-image))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when radius-p `(%:radius ,radius))
             ,@(when central-angle-p `(%:central-angle ,central-angle))
             ,@(when aspect-ratio-p `(%:aspect-ratio ,aspect-ratio)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-cylinder-khr %struct-types%)
      ':type-composition-layer-cylinder-khr)

(defmacro with-composition-layer-cube-khr ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (layer-flags nil
                                                       layer-flags-p)
                                                      (space nil space-p)
                                                      (eye-visibility nil
                                                       eye-visibility-p)
                                                      (swapchain nil
                                                       swapchain-p)
                                                      (image-array-index nil
                                                       image-array-index-p)
                                                      (orientation nil
                                                       orientation-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-cube-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:eye-visibility
                                %:swapchain
                                %:image-array-index
                                %:orientation)
                               ,pointer (:struct %:composition-layer-cube-khr))
       (setf %:type :type-composition-layer-cube-khr
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when eye-visibility-p `(%:eye-visibility ,eye-visibility))
             ,@(when swapchain-p `(%:swapchain ,swapchain))
             ,@(when image-array-index-p `(%:image-array-index ,image-array-index))
             ,@(when orientation-p `(%:orientation ,orientation)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-cube-khr %struct-types%)
      ':type-composition-layer-cube-khr)

(defmacro with-composition-layer-equirect-khr ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (layer-flags nil
                                                           layer-flags-p)
                                                          (space nil space-p)
                                                          (eye-visibility nil
                                                           eye-visibility-p)
                                                          (sub-image nil
                                                           sub-image-p)
                                                          (pose nil pose-p)
                                                          (radius nil radius-p)
                                                          (scale nil scale-p)
                                                          (bias nil bias-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-equirect-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:eye-visibility
                                %:sub-image
                                %:pose
                                %:radius
                                %:scale
                                %:bias)
                               ,pointer (:struct %:composition-layer-equirect-khr))
       (setf %:type :type-composition-layer-equirect-khr
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when eye-visibility-p `(%:eye-visibility ,eye-visibility))
             ,@(when sub-image-p `(%:sub-image ,sub-image))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when radius-p `(%:radius ,radius))
             ,@(when scale-p `(%:scale ,scale))
             ,@(when bias-p `(%:bias ,bias)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-equirect-khr %struct-types%)
      ':type-composition-layer-equirect-khr)

(defmacro with-composition-layer-depth-info-khr ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (sub-image nil
                                                             sub-image-p)
                                                            (min-depth nil
                                                             min-depth-p)
                                                            (max-depth nil
                                                             max-depth-p)
                                                            (near-z nil
                                                             near-z-p)
                                                            (far-z nil far-z-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-depth-info-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:sub-image
                                %:min-depth
                                %:max-depth
                                %:near-z
                                %:far-z)
                               ,pointer (:struct %:composition-layer-depth-info-khr))
       (setf %:type :type-composition-layer-depth-info-khr
             %:next ,next
             ,@(when sub-image-p `(%:sub-image ,sub-image))
             ,@(when min-depth-p `(%:min-depth ,min-depth))
             ,@(when max-depth-p `(%:max-depth ,max-depth))
             ,@(when near-z-p `(%:near-z ,near-z))
             ,@(when far-z-p `(%:far-z ,far-z)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-depth-info-khr %struct-types%)
      ':type-composition-layer-depth-info-khr)

(defmacro with-frame-begin-info ((pointer &key %slots
                                            (next '(cffi:null-pointer)))
                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:frame-begin-info))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:frame-begin-info))
       (setf %:type :type-frame-begin-info
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:frame-begin-info %struct-types%)
      ':type-frame-begin-info)

(defmacro with-frame-end-info ((pointer &key %slots (next '(cffi:null-pointer))
                                          (display-time nil display-time-p)
                                          (environment-blend-mode nil
                                           environment-blend-mode-p)
                                          (layer-count nil layer-count-p)
                                          (layers nil layers-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:frame-end-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:display-time
                                %:environment-blend-mode
                                %:layer-count
                                %:layers)
                               ,pointer (:struct %:frame-end-info))
       (setf %:type :type-frame-end-info
             %:next ,next
             ,@(when display-time-p `(%:display-time ,display-time))
             ,@(when environment-blend-mode-p `(%:environment-blend-mode ,environment-blend-mode))
             ,@(when layer-count-p `(%:layer-count ,layer-count))
             ,@(when layers-p `(%:layers ,layers)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:frame-end-info %struct-types%)
      ':type-frame-end-info)

(defmacro with-frame-wait-info ((pointer &key %slots
                                           (next '(cffi:null-pointer)))
                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:frame-wait-info))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:frame-wait-info))
       (setf %:type :type-frame-wait-info
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:frame-wait-info %struct-types%)
      ':type-frame-wait-info)

(defmacro with-frame-state ((pointer &key %slots (next '(cffi:null-pointer))
                                       (predicted-display-time nil
                                        predicted-display-time-p)
                                       (predicted-display-period nil
                                        predicted-display-period-p)
                                       (should-render nil should-render-p))
                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:frame-state))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:predicted-display-time
                                %:predicted-display-period
                                %:should-render)
                               ,pointer (:struct %:frame-state))
       (setf %:type :type-frame-state
             %:next ,next
             ,@(when predicted-display-time-p `(%:predicted-display-time ,predicted-display-time))
             ,@(when predicted-display-period-p `(%:predicted-display-period ,predicted-display-period))
             ,@(when should-render-p `(%:should-render ,should-render)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:frame-state %struct-types%)
      ':type-frame-state)

(defmacro with-haptic-vibration ((pointer &key %slots
                                            (next '(cffi:null-pointer))
                                            (duration nil duration-p)
                                            (frequency nil frequency-p)
                                            (amplitude nil amplitude-p))
                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:haptic-vibration))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:duration
                                %:frequency
                                %:amplitude)
                               ,pointer (:struct %:haptic-vibration))
       (setf %:type :type-haptic-vibration
             %:next ,next
             ,@(when duration-p `(%:duration ,duration))
             ,@(when frequency-p `(%:frequency ,frequency))
             ,@(when amplitude-p `(%:amplitude ,amplitude)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:haptic-vibration %struct-types%)
      ':type-haptic-vibration)

(defmacro with-event-data-buffer ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (varying nil varying-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-buffer))
     (cffi:with-foreign-slots ((%:type %:next %:varying)
                               ,pointer (:struct %:event-data-buffer))
       (setf %:type :type-event-data-buffer
             %:next ,next
             ,@(when varying-p `(%:varying ,varying)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-buffer %struct-types%)
      ':type-event-data-buffer)

(defmacro with-event-data-events-lost ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (lost-event-count nil
                                                   lost-event-count-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-events-lost))
     (cffi:with-foreign-slots ((%:type %:next %:lost-event-count)
                               ,pointer (:struct %:event-data-events-lost))
       (setf %:type :type-event-data-events-lost
             %:next ,next
             ,@(when lost-event-count-p `(%:lost-event-count ,lost-event-count)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-events-lost %struct-types%)
      ':type-event-data-events-lost)

(defmacro with-event-data-instance-loss-pending ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (loss-time nil
                                                             loss-time-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-instance-loss-pending))
     (cffi:with-foreign-slots ((%:type %:next %:loss-time)
                               ,pointer (:struct %:event-data-instance-loss-pending))
       (setf %:type :type-event-data-instance-loss-pending
             %:next ,next
             ,@(when loss-time-p `(%:loss-time ,loss-time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-instance-loss-pending %struct-types%)
      ':type-event-data-instance-loss-pending)

(defmacro with-event-data-session-state-changed ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (session nil
                                                             session-p)
                                                            (state nil state-p)
                                                            (time nil time-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-session-state-changed))
     (cffi:with-foreign-slots ((%:type %:next %:session %:state %:time)
                               ,pointer (:struct %:event-data-session-state-changed))
       (setf %:type :type-event-data-session-state-changed
             %:next ,next
             ,@(when session-p `(%:session ,session))
             ,@(when state-p `(%:state ,state))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-session-state-changed %struct-types%)
      ':type-event-data-session-state-changed)

(defmacro with-event-data-reference-space-change-pending ((pointer &key %slots
                                                                     (next
                                                                      '(cffi:null-pointer))
                                                                     (session
                                                                      nil
                                                                      session-p)
                                                                     (reference-space-type
                                                                      nil
                                                                      reference-space-type-p)
                                                                     (change-time
                                                                      nil
                                                                      change-time-p)
                                                                     (pose-valid
                                                                      nil
                                                                      pose-valid-p)
                                                                     (pose-in-previous-space
                                                                      nil
                                                                      pose-in-previous-space-p))
                                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-reference-space-change-pending))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:session
                                %:reference-space-type
                                %:change-time
                                %:pose-valid
                                %:pose-in-previous-space)
                               ,pointer (:struct %:event-data-reference-space-change-pending))
       (setf %:type :type-event-data-reference-space-change-pending
             %:next ,next
             ,@(when session-p `(%:session ,session))
             ,@(when reference-space-type-p `(%:reference-space-type ,reference-space-type))
             ,@(when change-time-p `(%:change-time ,change-time))
             ,@(when pose-valid-p `(%:pose-valid ,pose-valid))
             ,@(when pose-in-previous-space-p `(%:pose-in-previous-space ,pose-in-previous-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-reference-space-change-pending %struct-types%)
      ':type-event-data-reference-space-change-pending)

(defmacro with-event-data-perf-settings-ext ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer))
                                                        (domain nil domain-p)
                                                        (sub-domain nil
                                                         sub-domain-p)
                                                        (from-level nil
                                                         from-level-p)
                                                        (to-level nil
                                                         to-level-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-perf-settings-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:domain
                                %:sub-domain
                                %:from-level
                                %:to-level)
                               ,pointer (:struct %:event-data-perf-settings-ext))
       (setf %:type :type-event-data-perf-settings-ext
             %:next ,next
             ,@(when domain-p `(%:domain ,domain))
             ,@(when sub-domain-p `(%:sub-domain ,sub-domain))
             ,@(when from-level-p `(%:from-level ,from-level))
             ,@(when to-level-p `(%:to-level ,to-level)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-perf-settings-ext %struct-types%)
      ':type-event-data-perf-settings-ext)

(defmacro with-event-data-visibility-mask-changed-khr ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (session nil
                                                                   session-p)
                                                                  (view-configuration-type
                                                                   nil
                                                                   view-configuration-type-p)
                                                                  (view-index
                                                                   nil
                                                                   view-index-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-visibility-mask-changed-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:session
                                %:view-configuration-type
                                %:view-index)
                               ,pointer (:struct %:event-data-visibility-mask-changed-khr))
       (setf %:type :type-event-data-visibility-mask-changed-khr
             %:next ,next
             ,@(when session-p `(%:session ,session))
             ,@(when view-configuration-type-p `(%:view-configuration-type ,view-configuration-type))
             ,@(when view-index-p `(%:view-index ,view-index)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-visibility-mask-changed-khr %struct-types%)
      ':type-event-data-visibility-mask-changed-khr)

(defmacro with-view-configuration-properties ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (view-configuration-type
                                                          nil
                                                          view-configuration-type-p)
                                                         (fov-mutable nil
                                                          fov-mutable-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-configuration-properties))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-type
                                %:fov-mutable)
                               ,pointer (:struct %:view-configuration-properties))
       (setf %:type :type-view-configuration-properties
             %:next ,next
             ,@(when view-configuration-type-p `(%:view-configuration-type ,view-configuration-type))
             ,@(when fov-mutable-p `(%:fov-mutable ,fov-mutable)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-configuration-properties %struct-types%)
      ':type-view-configuration-properties)

(defmacro with-action-state-boolean ((pointer &key %slots
                                                (next '(cffi:null-pointer))
                                                (current-state nil
                                                 current-state-p)
                                                (changed-since-last-sync nil
                                                 changed-since-last-sync-p)
                                                (last-change-time nil
                                                 last-change-time-p)
                                                (is-active nil is-active-p))
                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-state-boolean))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:current-state
                                %:changed-since-last-sync
                                %:last-change-time
                                %:is-active)
                               ,pointer (:struct %:action-state-boolean))
       (setf %:type :type-action-state-boolean
             %:next ,next
             ,@(when current-state-p `(%:current-state ,current-state))
             ,@(when changed-since-last-sync-p `(%:changed-since-last-sync ,changed-since-last-sync))
             ,@(when last-change-time-p `(%:last-change-time ,last-change-time))
             ,@(when is-active-p `(%:is-active ,is-active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-state-boolean %struct-types%)
      ':type-action-state-boolean)

(defmacro with-action-state-float ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (current-state nil
                                               current-state-p)
                                              (changed-since-last-sync nil
                                               changed-since-last-sync-p)
                                              (last-change-time nil
                                               last-change-time-p)
                                              (is-active nil is-active-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-state-float))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:current-state
                                %:changed-since-last-sync
                                %:last-change-time
                                %:is-active)
                               ,pointer (:struct %:action-state-float))
       (setf %:type :type-action-state-float
             %:next ,next
             ,@(when current-state-p `(%:current-state ,current-state))
             ,@(when changed-since-last-sync-p `(%:changed-since-last-sync ,changed-since-last-sync))
             ,@(when last-change-time-p `(%:last-change-time ,last-change-time))
             ,@(when is-active-p `(%:is-active ,is-active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-state-float %struct-types%)
      ':type-action-state-float)

(defmacro with-action-state-vector-2f ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (current-state nil
                                                   current-state-p)
                                                  (changed-since-last-sync nil
                                                   changed-since-last-sync-p)
                                                  (last-change-time nil
                                                   last-change-time-p)
                                                  (is-active nil is-active-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-state-vector-2f))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:current-state
                                %:changed-since-last-sync
                                %:last-change-time
                                %:is-active)
                               ,pointer (:struct %:action-state-vector-2f))
       (setf %:type :type-action-state-vector2f
             %:next ,next
             ,@(when current-state-p `(%:current-state ,current-state))
             ,@(when changed-since-last-sync-p `(%:changed-since-last-sync ,changed-since-last-sync))
             ,@(when last-change-time-p `(%:last-change-time ,last-change-time))
             ,@(when is-active-p `(%:is-active ,is-active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-state-vector-2f %struct-types%)
      ':type-action-state-vector2f)

(defmacro with-action-state-pose ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (is-active nil is-active-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-state-pose))
     (cffi:with-foreign-slots ((%:type %:next %:is-active)
                               ,pointer (:struct %:action-state-pose))
       (setf %:type :type-action-state-pose
             %:next ,next
             ,@(when is-active-p `(%:is-active ,is-active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-state-pose %struct-types%)
      ':type-action-state-pose)

(defmacro with-action-state-get-info ((pointer &key %slots
                                                 (next '(cffi:null-pointer))
                                                 (action nil action-p)
                                                 (subaction-path %:+null-path+))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-state-get-info))
     (cffi:with-foreign-slots ((%:type %:next %:action %:subaction-path)
                               ,pointer (:struct %:action-state-get-info))
       (setf %:type :type-action-state-get-info
             %:next ,next
             ,@(when action-p `(%:action ,action))
             %:subaction-path ,subaction-path)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-state-get-info %struct-types%)
      ':type-action-state-get-info)

(defmacro with-haptic-action-info ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (action nil action-p)
                                              (subaction-path %:+null-path+))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:haptic-action-info))
     (cffi:with-foreign-slots ((%:type %:next %:action %:subaction-path)
                               ,pointer (:struct %:haptic-action-info))
       (setf %:type :type-haptic-action-info
             %:next ,next
             ,@(when action-p `(%:action ,action))
             %:subaction-path ,subaction-path)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:haptic-action-info %struct-types%)
      ':type-haptic-action-info)

(defmacro with-action-set-create-info ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (action-set-name nil
                                                   action-set-name-p)
                                                  (localized-action-set-name
                                                   nil
                                                   localized-action-set-name-p)
                                                  (priority 0))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-set-create-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:action-set-name
                                %:localized-action-set-name
                                %:priority)
                               ,pointer (:struct %:action-set-create-info))
       (setf %:type :type-action-set-create-info
             %:next ,next
             %:priority ,priority)
       (if (and ,action-set-name-p (not ,action-set-name))
           (setf %:action-set-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,action-set-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:action-set-create-info) '%:action-set-name)
            %:+max-action-set-name-size+ :encoding :utf-8))
       (if (and ,localized-action-set-name-p (not ,localized-action-set-name))
           (setf %:localized-action-set-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,localized-action-set-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:action-set-create-info) '%:localized-action-set-name)
            %:+max-localized-action-set-name-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-set-create-info %struct-types%)
      ':type-action-set-create-info)

(defmacro with-action-suggested-binding ((pointer &key %slots
                                                    (action nil action-p)
                                                    (binding nil binding-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-suggested-binding))
     (cffi:with-foreign-slots ((%:action %:binding)
                               ,pointer (:struct %:action-suggested-binding))
       (setf ,@(when action-p `(%:action ,action))
             ,@(when binding-p `(%:binding ,binding)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-interaction-profile-suggested-binding ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (interaction-profile
                                                                  nil
                                                                  interaction-profile-p)
                                                                 (count-suggested-bindings
                                                                  nil
                                                                  count-suggested-bindings-p)
                                                                 (suggested-bindings
                                                                  nil
                                                                  suggested-bindings-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:interaction-profile-suggested-binding))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:interaction-profile
                                %:count-suggested-bindings
                                %:suggested-bindings)
                               ,pointer (:struct %:interaction-profile-suggested-binding))
       (setf %:type :type-interaction-profile-suggested-binding
             %:next ,next
             ,@(when interaction-profile-p `(%:interaction-profile ,interaction-profile))
             ,@(when count-suggested-bindings-p `(%:count-suggested-bindings ,count-suggested-bindings))
             ,@(when suggested-bindings-p `(%:suggested-bindings ,suggested-bindings)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:interaction-profile-suggested-binding %struct-types%)
      ':type-interaction-profile-suggested-binding)

(defmacro with-active-action-set ((pointer &key %slots
                                             (action-set nil action-set-p)
                                             (subaction-path %:+null-path+))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:active-action-set))
     (cffi:with-foreign-slots ((%:action-set %:subaction-path)
                               ,pointer (:struct %:active-action-set))
       (setf ,@(when action-set-p `(%:action-set ,action-set))
             %:subaction-path ,subaction-path)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-session-action-sets-attach-info ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (count-action-sets
                                                            nil
                                                            count-action-sets-p)
                                                           (action-sets nil
                                                            action-sets-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:session-action-sets-attach-info))
     (cffi:with-foreign-slots ((%:type %:next %:count-action-sets %:action-sets)
                               ,pointer (:struct %:session-action-sets-attach-info))
       (setf %:type :type-session-action-sets-attach-info
             %:next ,next
             ,@(when count-action-sets-p `(%:count-action-sets ,count-action-sets))
             ,@(when action-sets-p `(%:action-sets ,action-sets)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:session-action-sets-attach-info %struct-types%)
      ':type-session-action-sets-attach-info)

(defmacro with-actions-sync-info ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (count-active-action-sets nil
                                              count-active-action-sets-p)
                                             (active-action-sets nil
                                              active-action-sets-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:actions-sync-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:count-active-action-sets
                                %:active-action-sets)
                               ,pointer (:struct %:actions-sync-info))
       (setf %:type :type-actions-sync-info
             %:next ,next
             ,@(when count-active-action-sets-p `(%:count-active-action-sets ,count-active-action-sets))
             ,@(when active-action-sets-p `(%:active-action-sets ,active-action-sets)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:actions-sync-info %struct-types%)
      ':type-actions-sync-info)

(defmacro with-bound-sources-for-action-enumerate-info ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (action nil
                                                                    action-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:bound-sources-for-action-enumerate-info))
     (cffi:with-foreign-slots ((%:type %:next %:action)
                               ,pointer (:struct %:bound-sources-for-action-enumerate-info))
       (setf %:type :type-bound-sources-for-action-enumerate-info
             %:next ,next
             ,@(when action-p `(%:action ,action)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:bound-sources-for-action-enumerate-info %struct-types%)
      ':type-bound-sources-for-action-enumerate-info)

(defmacro with-input-source-localized-name-get-info ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (source-path
                                                                 nil
                                                                 source-path-p)
                                                                (which-components
                                                                 nil
                                                                 which-components-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:input-source-localized-name-get-info))
     (cffi:with-foreign-slots ((%:type %:next %:source-path %:which-components)
                               ,pointer (:struct %:input-source-localized-name-get-info))
       (setf %:type :type-input-source-localized-name-get-info
             %:next ,next
             ,@(when source-path-p `(%:source-path ,source-path))
             ,@(when which-components-p `(%:which-components ,which-components)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:input-source-localized-name-get-info %struct-types%)
      ':type-input-source-localized-name-get-info)

(defmacro with-event-data-interaction-profile-changed ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (session nil
                                                                   session-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-interaction-profile-changed))
     (cffi:with-foreign-slots ((%:type %:next %:session)
                               ,pointer (:struct %:event-data-interaction-profile-changed))
       (setf %:type :type-event-data-interaction-profile-changed
             %:next ,next
             ,@(when session-p `(%:session ,session)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-interaction-profile-changed %struct-types%)
      ':type-event-data-interaction-profile-changed)

(defmacro with-interaction-profile-state ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (interaction-profile nil
                                                                          interaction-profile-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:interaction-profile-state))
     (cffi:with-foreign-slots ((%:type %:next %:interaction-profile)
                               ,pointer (:struct %:interaction-profile-state))
       (setf %:type :type-interaction-profile-state
             %:next ,next
             ,@(when interaction-profile-p `(%:interaction-profile ,interaction-profile)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:interaction-profile-state %struct-types%)
      ':type-interaction-profile-state)

(defmacro with-action-create-info ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (action-name nil action-name-p)
                                              (action-type nil action-type-p)
                                              (count-subaction-paths nil
                                               count-subaction-paths-p)
                                              (subaction-paths nil
                                               subaction-paths-p)
                                              (localized-action-name nil
                                               localized-action-name-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:action-create-info))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:action-name
                                %:action-type
                                %:count-subaction-paths
                                %:subaction-paths
                                %:localized-action-name)
                               ,pointer (:struct %:action-create-info))
       (setf %:type :type-action-create-info
             %:next ,next
             ,@(when action-type-p `(%:action-type ,action-type))
             ,@(when count-subaction-paths-p `(%:count-subaction-paths ,count-subaction-paths))
             ,@(when subaction-paths-p `(%:subaction-paths ,subaction-paths)))
       (if (and ,action-name-p (not ,action-name))
           (setf %:action-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,action-name "")
            (cffi:foreign-slot-pointer ,pointer '(:struct %:action-create-info)
             '%:action-name)
            %:+max-action-name-size+ :encoding :utf-8))
       (if (and ,localized-action-name-p (not ,localized-action-name))
           (setf %:localized-action-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,localized-action-name "")
            (cffi:foreign-slot-pointer ,pointer '(:struct %:action-create-info)
             '%:localized-action-name)
            %:+max-localized-action-name-size+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:action-create-info %struct-types%)
      ':type-action-create-info)

(defmacro with-instance-create-info-android-khr ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (application-vm nil
                                                             application-vm-p)
                                                            (application-activity
                                                             nil
                                                             application-activity-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:instance-create-info-android-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:application-vm
                                %:application-activity)
                               ,pointer (:struct %:instance-create-info-android-khr))
       (setf %:type :type-instance-create-info-android-khr
             %:next ,next
             ,@(when application-vm-p `(%:application-vm ,application-vm))
             ,@(when application-activity-p `(%:application-activity ,application-activity)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:instance-create-info-android-khr %struct-types%)
      ':type-instance-create-info-android-khr)

(defmacro with-vulkan-swapchain-format-list-create-info-khr ((pointer &key
                                                                        %slots
                                                                        (next
                                                                         '(cffi:null-pointer))
                                                                        (view-format-count
                                                                         nil
                                                                         view-format-count-p)
                                                                        (view-formats
                                                                         nil
                                                                         view-formats-p))
                                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vulkan-swapchain-format-list-create-info-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-format-count
                                %:view-formats)
                               ,pointer (:struct %:vulkan-swapchain-format-list-create-info-khr))
       (setf %:type :type-vulkan-swapchain-format-list-create-info-khr
             %:next ,next
             ,@(when view-format-count-p `(%:view-format-count ,view-format-count))
             ,@(when view-formats-p `(%:view-formats ,view-formats)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:vulkan-swapchain-format-list-create-info-khr %struct-types%)
      ':type-vulkan-swapchain-format-list-create-info-khr)

(defmacro with-debug-utils-object-name-info-ext ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (object-type nil
                                                             object-type-p)
                                                            (object-handle nil
                                                             object-handle-p)
                                                            (object-name nil
                                                             object-name-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:debug-utils-object-name-info-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:object-type
                                %:object-handle
                                %:object-name)
                               ,pointer (:struct %:debug-utils-object-name-info-ext))
       (setf %:type :type-debug-utils-object-name-info-ext
             %:next ,next
             ,@(when object-type-p `(%:object-type ,object-type))
             ,@(when object-handle-p `(%:object-handle ,object-handle))
             ,@(when object-name-p `(%:object-name ,object-name)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:debug-utils-object-name-info-ext %struct-types%)
      ':type-debug-utils-object-name-info-ext)

(defmacro with-debug-utils-label-ext ((pointer &key %slots
                                                 (next '(cffi:null-pointer))
                                                 (label-name nil label-name-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:debug-utils-label-ext))
     (cffi:with-foreign-slots ((%:type %:next %:label-name)
                               ,pointer (:struct %:debug-utils-label-ext))
       (setf %:type :type-debug-utils-label-ext
             %:next ,next
             ,@(when label-name-p `(%:label-name ,label-name)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:debug-utils-label-ext %struct-types%)
      ':type-debug-utils-label-ext)

(defmacro with-debug-utils-messenger-callback-data-ext ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (message-id
                                                                    nil
                                                                    message-id-p)
                                                                   (function-name
                                                                    nil
                                                                    function-name-p)
                                                                   (message nil
                                                                    message-p)
                                                                   (object-count
                                                                    nil
                                                                    object-count-p)
                                                                   (objects nil
                                                                    objects-p)
                                                                   (session-label-count
                                                                    nil
                                                                    session-label-count-p)
                                                                   (session-labels
                                                                    nil
                                                                    session-labels-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:debug-utils-messenger-callback-data-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:message-id
                                %:function-name
                                %:message
                                %:object-count
                                %:objects
                                %:session-label-count
                                %:session-labels)
                               ,pointer (:struct %:debug-utils-messenger-callback-data-ext))
       (setf %:type :type-debug-utils-messenger-callback-data-ext
             %:next ,next
             ,@(when message-id-p `(%:message-id ,message-id))
             ,@(when function-name-p `(%:function-name ,function-name))
             ,@(when message-p `(%:message ,message))
             ,@(when object-count-p `(%:object-count ,object-count))
             ,@(when objects-p `(%:objects ,objects))
             ,@(when session-label-count-p `(%:session-label-count ,session-label-count))
             ,@(when session-labels-p `(%:session-labels ,session-labels)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:debug-utils-messenger-callback-data-ext %struct-types%)
      ':type-debug-utils-messenger-callback-data-ext)

(defmacro with-debug-utils-messenger-create-info-ext ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (message-severities
                                                                  nil)
                                                                 (message-types
                                                                  nil)
                                                                 (user-callback
                                                                  nil)
                                                                 (user-data 0))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:debug-utils-messenger-create-info-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:message-severities
                                %:message-types
                                %:user-callback
                                %:user-data)
                               ,pointer (:struct %:debug-utils-messenger-create-info-ext))
       (setf %:type :type-debug-utils-messenger-create-info-ext
             %:next ,next
             %:message-severities ,message-severities
             %:message-types ,message-types
             %:user-callback (cast-callback ,user-callback)
             %:user-data (cast-int-pointer ,user-data))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:debug-utils-messenger-create-info-ext %struct-types%)
      ':type-debug-utils-messenger-create-info-ext)

(defmacro with-visibility-mask-khr ((pointer &key %slots
                                               (next '(cffi:null-pointer))
                                               (vertex-capacity-input nil
                                                vertex-capacity-input-p)
                                               (vertex-count-output nil
                                                vertex-count-output-p)
                                               (vertices nil vertices-p)
                                               (index-capacity-input nil
                                                index-capacity-input-p)
                                               (index-count-output nil
                                                index-count-output-p)
                                               (indices nil indices-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:visibility-mask-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:vertex-capacity-input
                                %:vertex-count-output
                                %:vertices
                                %:index-capacity-input
                                %:index-count-output
                                %:indices)
                               ,pointer (:struct %:visibility-mask-khr))
       (setf %:type :type-visibility-mask-khr
             %:next ,next
             ,@(when vertex-capacity-input-p `(%:vertex-capacity-input ,vertex-capacity-input))
             ,@(when vertex-count-output-p `(%:vertex-count-output ,vertex-count-output))
             ,@(when vertices-p `(%:vertices ,vertices))
             ,@(when index-capacity-input-p `(%:index-capacity-input ,index-capacity-input))
             ,@(when index-count-output-p `(%:index-count-output ,index-count-output))
             ,@(when indices-p `(%:indices ,indices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:visibility-mask-khr %struct-types%)
      ':type-visibility-mask-khr)

(defmacro with-graphics-requirements-opengl-khr ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (min-api-version-supported
                                                             nil
                                                             min-api-version-supported-p)
                                                            (max-api-version-supported
                                                             nil
                                                             max-api-version-supported-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-requirements-opengl-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:min-api-version-supported
                                %:max-api-version-supported)
                               ,pointer (:struct %:graphics-requirements-opengl-khr))
       (setf %:type :type-graphics-requirements-opengl-khr
             %:next ,next
             ,@(when min-api-version-supported-p `(%:min-api-version-supported ,min-api-version-supported))
             ,@(when max-api-version-supported-p `(%:max-api-version-supported ,max-api-version-supported)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-requirements-opengl-khr %struct-types%)
      ':type-graphics-requirements-opengl-khr)

(defmacro with-graphics-requirements-opengl-es-khr ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (min-api-version-supported
                                                                nil
                                                                min-api-version-supported-p)
                                                               (max-api-version-supported
                                                                nil
                                                                max-api-version-supported-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-requirements-opengl-es-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:min-api-version-supported
                                %:max-api-version-supported)
                               ,pointer (:struct %:graphics-requirements-opengl-es-khr))
       (setf %:type :type-graphics-requirements-opengl-es-khr
             %:next ,next
             ,@(when min-api-version-supported-p `(%:min-api-version-supported ,min-api-version-supported))
             ,@(when max-api-version-supported-p `(%:max-api-version-supported ,max-api-version-supported)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-requirements-opengl-es-khr %struct-types%)
      ':type-graphics-requirements-opengl-es-khr)

(defmacro with-graphics-requirements-vulkan-khr ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (min-api-version-supported
                                                             nil
                                                             min-api-version-supported-p)
                                                            (max-api-version-supported
                                                             nil
                                                             max-api-version-supported-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-requirements-vulkan-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:min-api-version-supported
                                %:max-api-version-supported)
                               ,pointer (:struct %:graphics-requirements-vulkan-khr))
       (setf %:type :type-graphics-requirements-vulkan-khr
             %:next ,next
             ,@(when min-api-version-supported-p `(%:min-api-version-supported ,min-api-version-supported))
             ,@(when max-api-version-supported-p `(%:max-api-version-supported ,max-api-version-supported)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-requirements-vulkan-khr %struct-types%)
      ':type-graphics-requirements-vulkan-khr)

(defmacro with-graphics-requirements-d3d11-khr ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (adapter-luid nil
                                                            adapter-luid-p)
                                                           (min-feature-level
                                                            nil
                                                            min-feature-level-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-requirements-d3d11-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:adapter-luid
                                %:min-feature-level)
                               ,pointer (:struct %:graphics-requirements-d3d11-khr))
       (setf %:type :type-graphics-requirements-d3d11-khr
             %:next ,next
             ,@(when adapter-luid-p `(%:adapter-luid ,adapter-luid))
             ,@(when min-feature-level-p `(%:min-feature-level ,min-feature-level)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-requirements-d3d11-khr %struct-types%)
      ':type-graphics-requirements-d3d11-khr)

(defmacro with-graphics-requirements-d3d12-khr ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (adapter-luid nil
                                                            adapter-luid-p)
                                                           (min-feature-level
                                                            nil
                                                            min-feature-level-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-requirements-d3d12-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:adapter-luid
                                %:min-feature-level)
                               ,pointer (:struct %:graphics-requirements-d3d12-khr))
       (setf %:type :type-graphics-requirements-d3d12-khr
             %:next ,next
             ,@(when adapter-luid-p `(%:adapter-luid ,adapter-luid))
             ,@(when min-feature-level-p `(%:min-feature-level ,min-feature-level)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-requirements-d3d12-khr %struct-types%)
      ':type-graphics-requirements-d3d12-khr)

(defmacro with-vulkan-instance-create-info-khr ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (system-id nil
                                                            system-id-p)
                                                           (create-flags nil
                                                            create-flags-p)
                                                           (pfn-get-instance-proc-addr
                                                            nil
                                                            pfn-get-instance-proc-addr-p)
                                                           (vulkan-create-info
                                                            nil
                                                            vulkan-create-info-p)
                                                           (vulkan-allocator
                                                            nil
                                                            vulkan-allocator-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vulkan-instance-create-info-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:system-id
                                %:create-flags
                                %:pfn-get-instance-proc-addr
                                %:vulkan-create-info
                                %:vulkan-allocator)
                               ,pointer (:struct %:vulkan-instance-create-info-khr))
       (setf %:type :type-vulkan-instance-create-info-khr
             %:next ,next
             ,@(when system-id-p `(%:system-id ,system-id))
             ,@(when create-flags-p `(%:create-flags ,create-flags))
             ,@(when pfn-get-instance-proc-addr-p `(%:pfn-get-instance-proc-addr ,pfn-get-instance-proc-addr))
             ,@(when vulkan-create-info-p `(%:vulkan-create-info ,vulkan-create-info))
             ,@(when vulkan-allocator-p `(%:vulkan-allocator ,vulkan-allocator)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:vulkan-instance-create-info-khr %struct-types%)
      ':type-vulkan-instance-create-info-khr)

(defmacro with-vulkan-device-create-info-khr ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (system-id nil
                                                          system-id-p)
                                                         (create-flags nil
                                                          create-flags-p)
                                                         (pfn-get-instance-proc-addr
                                                          nil
                                                          pfn-get-instance-proc-addr-p)
                                                         (vulkan-physical-device
                                                          nil
                                                          vulkan-physical-device-p)
                                                         (vulkan-create-info
                                                          nil
                                                          vulkan-create-info-p)
                                                         (vulkan-allocator nil
                                                          vulkan-allocator-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vulkan-device-create-info-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:system-id
                                %:create-flags
                                %:pfn-get-instance-proc-addr
                                %:vulkan-physical-device
                                %:vulkan-create-info
                                %:vulkan-allocator)
                               ,pointer (:struct %:vulkan-device-create-info-khr))
       (setf %:type :type-vulkan-device-create-info-khr
             %:next ,next
             ,@(when system-id-p `(%:system-id ,system-id))
             ,@(when create-flags-p `(%:create-flags ,create-flags))
             ,@(when pfn-get-instance-proc-addr-p `(%:pfn-get-instance-proc-addr ,pfn-get-instance-proc-addr))
             ,@(when vulkan-physical-device-p `(%:vulkan-physical-device ,vulkan-physical-device))
             ,@(when vulkan-create-info-p `(%:vulkan-create-info ,vulkan-create-info))
             ,@(when vulkan-allocator-p `(%:vulkan-allocator ,vulkan-allocator)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:vulkan-device-create-info-khr %struct-types%)
      ':type-vulkan-device-create-info-khr)

(defmacro with-vulkan-graphics-device-get-info-khr ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (system-id nil
                                                                system-id-p)
                                                               (vulkan-instance
                                                                nil
                                                                vulkan-instance-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vulkan-graphics-device-get-info-khr))
     (cffi:with-foreign-slots ((%:type %:next %:system-id %:vulkan-instance)
                               ,pointer (:struct %:vulkan-graphics-device-get-info-khr))
       (setf %:type :type-vulkan-graphics-device-get-info-khr
             %:next ,next
             ,@(when system-id-p `(%:system-id ,system-id))
             ,@(when vulkan-instance-p `(%:vulkan-instance ,vulkan-instance)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:vulkan-graphics-device-get-info-khr %struct-types%)
      ':type-vulkan-graphics-device-get-info-khr)

(defmacro with-vulkan-swapchain-create-info-meta ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (additional-create-flags
                                                              nil
                                                              additional-create-flags-p)
                                                             (additional-usage-flags
                                                              nil
                                                              additional-usage-flags-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vulkan-swapchain-create-info-meta))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:additional-create-flags
                                %:additional-usage-flags)
                               ,pointer (:struct %:vulkan-swapchain-create-info-meta))
       (setf %:type :type-vulkan-swapchain-create-info-meta
             %:next ,next
             ,@(when additional-create-flags-p `(%:additional-create-flags ,additional-create-flags))
             ,@(when additional-usage-flags-p `(%:additional-usage-flags ,additional-usage-flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:vulkan-swapchain-create-info-meta %struct-types%)
      ':type-vulkan-swapchain-create-info-meta)

(defmacro with-session-create-info-overlay-extx ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (create-flags nil
                                                             create-flags-p)
                                                            (session-layers-placement
                                                             nil
                                                             session-layers-placement-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:session-create-info-overlay-extx))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:create-flags
                                %:session-layers-placement)
                               ,pointer (:struct %:session-create-info-overlay-extx))
       (setf %:type :type-session-create-info-overlay-extx
             %:next ,next
             ,@(when create-flags-p `(%:create-flags ,create-flags))
             ,@(when session-layers-placement-p `(%:session-layers-placement ,session-layers-placement)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:session-create-info-overlay-extx %struct-types%)
      ':type-session-create-info-overlay-extx)

(defmacro with-event-data-main-session-visibility-changed-extx ((pointer &key
                                                                           %slots
                                                                           (next
                                                                            '(cffi:null-pointer))
                                                                           (visible
                                                                            nil
                                                                            visible-p)
                                                                           (flags
                                                                            nil
                                                                            flags-p))
                                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-main-session-visibility-changed-extx))
     (cffi:with-foreign-slots ((%:type %:next %:visible %:flags)
                               ,pointer (:struct %:event-data-main-session-visibility-changed-extx))
       (setf %:type :type-event-data-main-session-visibility-changed-extx
             %:next ,next
             ,@(when visible-p `(%:visible ,visible))
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-main-session-visibility-changed-extx %struct-types%)
      ':type-event-data-main-session-visibility-changed-extx)

(defmacro with-event-data-display-refresh-rate-changed-fb ((pointer &key %slots
                                                                      (next
                                                                       '(cffi:null-pointer))
                                                                      (from-display-refresh-rate
                                                                       nil
                                                                       from-display-refresh-rate-p)
                                                                      (to-display-refresh-rate
                                                                       nil
                                                                       to-display-refresh-rate-p))
                                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-display-refresh-rate-changed-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:from-display-refresh-rate
                                %:to-display-refresh-rate)
                               ,pointer (:struct %:event-data-display-refresh-rate-changed-fb))
       (setf %:type :type-event-data-display-refresh-rate-changed-fb
             %:next ,next
             ,@(when from-display-refresh-rate-p `(%:from-display-refresh-rate ,from-display-refresh-rate))
             ,@(when to-display-refresh-rate-p `(%:to-display-refresh-rate ,to-display-refresh-rate)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-display-refresh-rate-changed-fb %struct-types%)
      ':type-event-data-display-refresh-rate-changed-fb)

(defmacro with-view-configuration-depth-range-ext ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (recommended-near-z
                                                               nil
                                                               recommended-near-z-p)
                                                              (min-near-z nil
                                                               min-near-z-p)
                                                              (recommended-far-z
                                                               nil
                                                               recommended-far-z-p)
                                                              (max-far-z nil
                                                               max-far-z-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-configuration-depth-range-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:recommended-near-z
                                %:min-near-z
                                %:recommended-far-z
                                %:max-far-z)
                               ,pointer (:struct %:view-configuration-depth-range-ext))
       (setf %:type :type-view-configuration-depth-range-ext
             %:next ,next
             ,@(when recommended-near-z-p `(%:recommended-near-z ,recommended-near-z))
             ,@(when min-near-z-p `(%:min-near-z ,min-near-z))
             ,@(when recommended-far-z-p `(%:recommended-far-z ,recommended-far-z))
             ,@(when max-far-z-p `(%:max-far-z ,max-far-z)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-configuration-depth-range-ext %struct-types%)
      ':type-view-configuration-depth-range-ext)

(defmacro with-view-configuration-view-fov-epic ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (recommended-fov
                                                             nil
                                                             recommended-fov-p)
                                                            (max-mutable-fov
                                                             nil
                                                             max-mutable-fov-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-configuration-view-fov-epic))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:recommended-fov
                                %:max-mutable-fov)
                               ,pointer (:struct %:view-configuration-view-fov-epic))
       (setf %:type :type-view-configuration-view-fov-epic
             %:next ,next
             ,@(when recommended-fov-p `(%:recommended-fov ,recommended-fov))
             ,@(when max-mutable-fov-p `(%:max-mutable-fov ,max-mutable-fov)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-configuration-view-fov-epic %struct-types%)
      ':type-view-configuration-view-fov-epic)

(defmacro with-interaction-profile-dpad-binding-ext ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (binding nil
                                                                 binding-p)
                                                                (action-set nil
                                                                 action-set-p)
                                                                (force-threshold
                                                                 nil
                                                                 force-threshold-p)
                                                                (force-threshold-released
                                                                 nil
                                                                 force-threshold-released-p)
                                                                (center-region
                                                                 nil
                                                                 center-region-p)
                                                                (wedge-angle
                                                                 nil
                                                                 wedge-angle-p)
                                                                (is-sticky nil
                                                                 is-sticky-p)
                                                                (on-haptic nil
                                                                 on-haptic-p)
                                                                (off-haptic nil
                                                                 off-haptic-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:interaction-profile-dpad-binding-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:binding
                                %:action-set
                                %:force-threshold
                                %:force-threshold-released
                                %:center-region
                                %:wedge-angle
                                %:is-sticky
                                %:on-haptic
                                %:off-haptic)
                               ,pointer (:struct %:interaction-profile-dpad-binding-ext))
       (setf %:type :type-interaction-profile-dpad-binding-ext
             %:next ,next
             ,@(when binding-p `(%:binding ,binding))
             ,@(when action-set-p `(%:action-set ,action-set))
             ,@(when force-threshold-p `(%:force-threshold ,force-threshold))
             ,@(when force-threshold-released-p `(%:force-threshold-released ,force-threshold-released))
             ,@(when center-region-p `(%:center-region ,center-region))
             ,@(when wedge-angle-p `(%:wedge-angle ,wedge-angle))
             ,@(when is-sticky-p `(%:is-sticky ,is-sticky))
             ,@(when on-haptic-p `(%:on-haptic ,on-haptic))
             ,@(when off-haptic-p `(%:off-haptic ,off-haptic)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:interaction-profile-dpad-binding-ext %struct-types%)
      ':type-interaction-profile-dpad-binding-ext)

(defmacro with-interaction-profile-analog-threshold-valve ((pointer &key %slots
                                                                      (next
                                                                       '(cffi:null-pointer))
                                                                      (action
                                                                       nil
                                                                       action-p)
                                                                      (binding
                                                                       nil
                                                                       binding-p)
                                                                      (on-threshold
                                                                       nil
                                                                       on-threshold-p)
                                                                      (off-threshold
                                                                       nil
                                                                       off-threshold-p)
                                                                      (on-haptic
                                                                       nil
                                                                       on-haptic-p)
                                                                      (off-haptic
                                                                       nil
                                                                       off-haptic-p))
                                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:interaction-profile-analog-threshold-valve))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:action
                                %:binding
                                %:on-threshold
                                %:off-threshold
                                %:on-haptic
                                %:off-haptic)
                               ,pointer (:struct %:interaction-profile-analog-threshold-valve))
       (setf %:type :type-interaction-profile-analog-threshold-valve
             %:next ,next
             ,@(when action-p `(%:action ,action))
             ,@(when binding-p `(%:binding ,binding))
             ,@(when on-threshold-p `(%:on-threshold ,on-threshold))
             ,@(when off-threshold-p `(%:off-threshold ,off-threshold))
             ,@(when on-haptic-p `(%:on-haptic ,on-haptic))
             ,@(when off-haptic-p `(%:off-haptic ,off-haptic)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:interaction-profile-analog-threshold-valve %struct-types%)
      ':type-interaction-profile-analog-threshold-valve)

(defmacro with-binding-modifications-khr ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (binding-modification-count
                                                      nil
                                                      binding-modification-count-p)
                                                     (binding-modifications nil
                                                      binding-modifications-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:binding-modifications-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:binding-modification-count
                                %:binding-modifications)
                               ,pointer (:struct %:binding-modifications-khr))
       (setf %:type :type-binding-modifications-khr
             %:next ,next
             ,@(when binding-modification-count-p `(%:binding-modification-count ,binding-modification-count))
             ,@(when binding-modifications-p `(%:binding-modifications ,binding-modifications)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:binding-modifications-khr %struct-types%)
      ':type-binding-modifications-khr)

(defmacro with-system-eye-gaze-interaction-properties-ext ((pointer &key %slots
                                                                      (next
                                                                       '(cffi:null-pointer))
                                                                      (supports-eye-gaze-interaction
                                                                       nil
                                                                       supports-eye-gaze-interaction-p))
                                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-eye-gaze-interaction-properties-ext))
     (cffi:with-foreign-slots ((%:type %:next %:supports-eye-gaze-interaction)
                               ,pointer (:struct %:system-eye-gaze-interaction-properties-ext))
       (setf %:type :type-system-eye-gaze-interaction-properties-ext
             %:next ,next
             ,@(when supports-eye-gaze-interaction-p `(%:supports-eye-gaze-interaction ,supports-eye-gaze-interaction)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-eye-gaze-interaction-properties-ext %struct-types%)
      ':type-system-eye-gaze-interaction-properties-ext)

(defmacro with-eye-gaze-sample-time-ext ((pointer &key %slots
                                                    (next '(cffi:null-pointer))
                                                    (time nil time-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:eye-gaze-sample-time-ext))
     (cffi:with-foreign-slots ((%:type %:next %:time)
                               ,pointer (:struct %:eye-gaze-sample-time-ext))
       (setf %:type :type-eye-gaze-sample-time-ext
             %:next ,next
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:eye-gaze-sample-time-ext %struct-types%)
      ':type-eye-gaze-sample-time-ext)

(defmacro with-spatial-anchor-create-info-msft ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (space nil space-p)
                                                           (pose nil pose-p)
                                                           (time nil time-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-anchor-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:space %:pose %:time)
                               ,pointer (:struct %:spatial-anchor-create-info-msft))
       (setf %:type :type-spatial-anchor-create-info-msft
             %:next ,next
             ,@(when space-p `(%:space ,space))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-anchor-create-info-msft %struct-types%)
      ':type-spatial-anchor-create-info-msft)

(defmacro with-spatial-anchor-space-create-info-msft ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (anchor nil
                                                                  anchor-p)
                                                                 (pose-in-anchor-space
                                                                  nil
                                                                  pose-in-anchor-space-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-anchor-space-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:anchor %:pose-in-anchor-space)
                               ,pointer (:struct %:spatial-anchor-space-create-info-msft))
       (setf %:type :type-spatial-anchor-space-create-info-msft
             %:next ,next
             ,@(when anchor-p `(%:anchor ,anchor))
             ,@(when pose-in-anchor-space-p `(%:pose-in-anchor-space ,pose-in-anchor-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-anchor-space-create-info-msft %struct-types%)
      ':type-spatial-anchor-space-create-info-msft)

(defmacro with-composition-layer-image-layout-fb ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (flags nil
                                                              flags-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-image-layout-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:composition-layer-image-layout-fb))
       (setf %:type :type-composition-layer-image-layout-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-image-layout-fb %struct-types%)
      ':type-composition-layer-image-layout-fb)

(defmacro with-composition-layer-alpha-blend-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (src-factor-color
                                                             nil
                                                             src-factor-color-p)
                                                            (dst-factor-color
                                                             nil
                                                             dst-factor-color-p)
                                                            (src-factor-alpha
                                                             nil
                                                             src-factor-alpha-p)
                                                            (dst-factor-alpha
                                                             nil
                                                             dst-factor-alpha-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-alpha-blend-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:src-factor-color
                                %:dst-factor-color
                                %:src-factor-alpha
                                %:dst-factor-alpha)
                               ,pointer (:struct %:composition-layer-alpha-blend-fb))
       (setf %:type :type-composition-layer-alpha-blend-fb
             %:next ,next
             ,@(when src-factor-color-p `(%:src-factor-color ,src-factor-color))
             ,@(when dst-factor-color-p `(%:dst-factor-color ,dst-factor-color))
             ,@(when src-factor-alpha-p `(%:src-factor-alpha ,src-factor-alpha))
             ,@(when dst-factor-alpha-p `(%:dst-factor-alpha ,dst-factor-alpha)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-alpha-blend-fb %struct-types%)
      ':type-composition-layer-alpha-blend-fb)

(defmacro with-graphics-binding-eglmndx ((pointer &key %slots
                                                    (next '(cffi:null-pointer))
                                                    (get-proc-address nil
                                                     get-proc-address-p)
                                                    (display nil display-p)
                                                    (config nil config-p)
                                                    (context nil context-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:graphics-binding-eglmndx))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:get-proc-address
                                %:display
                                %:config
                                %:context)
                               ,pointer (:struct %:graphics-binding-eglmndx))
       (setf %:type :type-graphics-binding-egl-mndx
             %:next ,next
             ,@(when get-proc-address-p `(%:get-proc-address ,get-proc-address))
             ,@(when display-p `(%:display ,display))
             ,@(when config-p `(%:config ,config))
             ,@(when context-p `(%:context ,context)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:graphics-binding-eglmndx %struct-types%)
      ':type-graphics-binding-egl-mndx)

(defmacro with-spatial-graph-node-space-create-info-msft ((pointer &key %slots
                                                                     (next
                                                                      '(cffi:null-pointer))
                                                                     (node-type
                                                                      nil
                                                                      node-type-p)
                                                                     (node-id
                                                                      nil
                                                                      node-id-p)
                                                                     (pose nil
                                                                      pose-p))
                                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-graph-node-space-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:node-type %:node-id %:pose)
                               ,pointer (:struct %:spatial-graph-node-space-create-info-msft))
       (setf %:type :type-spatial-graph-node-space-create-info-msft
             %:next ,next
             ,@(when node-type-p `(%:node-type ,node-type))
             ,@(when node-id-p `(%:node-id ,node-id))
             ,@(when pose-p `(%:pose ,pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-graph-node-space-create-info-msft %struct-types%)
      ':type-spatial-graph-node-space-create-info-msft)

(defmacro with-spatial-graph-static-node-binding-create-info-msft ((pointer &key
                                                                              %slots
                                                                              (next
                                                                               '(cffi:null-pointer))
                                                                              (space
                                                                               nil
                                                                               space-p)
                                                                              (pose-in-space
                                                                               nil
                                                                               pose-in-space-p)
                                                                              (time
                                                                               nil
                                                                               time-p))
                                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-graph-static-node-binding-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:space %:pose-in-space %:time)
                               ,pointer (:struct %:spatial-graph-static-node-binding-create-info-msft))
       (setf %:type :type-spatial-graph-static-node-binding-create-info-msft
             %:next ,next
             ,@(when space-p `(%:space ,space))
             ,@(when pose-in-space-p `(%:pose-in-space ,pose-in-space))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-graph-static-node-binding-create-info-msft %struct-types%)
      ':type-spatial-graph-static-node-binding-create-info-msft)

(defmacro with-spatial-graph-node-binding-properties-get-info-msft ((pointer &key
                                                                               %slots
                                                                               (next
                                                                                '(cffi:null-pointer)))
                                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-graph-node-binding-properties-get-info-msft))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:spatial-graph-node-binding-properties-get-info-msft))
       (setf %:type :type-spatial-graph-node-binding-properties-get-info-msft
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-graph-node-binding-properties-get-info-msft %struct-types%)
      ':type-spatial-graph-node-binding-properties-get-info-msft)

(defmacro with-spatial-graph-node-binding-properties-msft ((pointer &key %slots
                                                                      (next
                                                                       '(cffi:null-pointer))
                                                                      (node-id
                                                                       nil
                                                                       node-id-p)
                                                                      (pose-in-node-space
                                                                       nil
                                                                       pose-in-node-space-p))
                                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-graph-node-binding-properties-msft))
     (cffi:with-foreign-slots ((%:type %:next %:node-id %:pose-in-node-space)
                               ,pointer (:struct %:spatial-graph-node-binding-properties-msft))
       (setf %:type :type-spatial-graph-node-binding-properties-msft
             %:next ,next
             ,@(when node-id-p `(%:node-id ,node-id))
             ,@(when pose-in-node-space-p `(%:pose-in-node-space ,pose-in-node-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-graph-node-binding-properties-msft %struct-types%)
      ':type-spatial-graph-node-binding-properties-msft)

(defmacro with-system-hand-tracking-properties-ext ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (supports-hand-tracking
                                                                nil
                                                                supports-hand-tracking-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-hand-tracking-properties-ext))
     (cffi:with-foreign-slots ((%:type %:next %:supports-hand-tracking)
                               ,pointer (:struct %:system-hand-tracking-properties-ext))
       (setf %:type :type-system-hand-tracking-properties-ext
             %:next ,next
             ,@(when supports-hand-tracking-p `(%:supports-hand-tracking ,supports-hand-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-hand-tracking-properties-ext %struct-types%)
      ':type-system-hand-tracking-properties-ext)

(defmacro with-hand-tracker-create-info-ext ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer))
                                                        (hand nil hand-p)
                                                        (hand-joint-set nil
                                                         hand-joint-set-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-tracker-create-info-ext))
     (cffi:with-foreign-slots ((%:type %:next %:hand %:hand-joint-set)
                               ,pointer (:struct %:hand-tracker-create-info-ext))
       (setf %:type :type-hand-tracker-create-info-ext
             %:next ,next
             ,@(when hand-p `(%:hand ,hand))
             ,@(when hand-joint-set-p `(%:hand-joint-set ,hand-joint-set)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-tracker-create-info-ext %struct-types%)
      ':type-hand-tracker-create-info-ext)

(defmacro with-hand-joints-locate-info-ext ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (base-space nil
                                                        base-space-p)
                                                       (time nil time-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-joints-locate-info-ext))
     (cffi:with-foreign-slots ((%:type %:next %:base-space %:time)
                               ,pointer (:struct %:hand-joints-locate-info-ext))
       (setf %:type :type-hand-joints-locate-info-ext
             %:next ,next
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-joints-locate-info-ext %struct-types%)
      ':type-hand-joints-locate-info-ext)

(defmacro with-hand-joint-location-ext ((pointer &key %slots
                                                   (location-flags nil
                                                    location-flags-p)
                                                   (pose nil pose-p)
                                                   (radius nil radius-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-joint-location-ext))
     (cffi:with-foreign-slots ((%:location-flags %:pose %:radius)
                               ,pointer (:struct %:hand-joint-location-ext))
       (setf ,@(when location-flags-p `(%:location-flags ,location-flags))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when radius-p `(%:radius ,radius)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-hand-joint-velocity-ext ((pointer &key %slots
                                                   (velocity-flags nil
                                                    velocity-flags-p)
                                                   (linear-velocity nil
                                                    linear-velocity-p)
                                                   (angular-velocity nil
                                                    angular-velocity-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-joint-velocity-ext))
     (cffi:with-foreign-slots ((%:velocity-flags
                                %:linear-velocity
                                %:angular-velocity)
                               ,pointer (:struct %:hand-joint-velocity-ext))
       (setf ,@(when velocity-flags-p `(%:velocity-flags ,velocity-flags))
             ,@(when linear-velocity-p `(%:linear-velocity ,linear-velocity))
             ,@(when angular-velocity-p `(%:angular-velocity ,angular-velocity)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-hand-joint-locations-ext ((pointer &key %slots
                                                    (next '(cffi:null-pointer))
                                                    (is-active nil is-active-p)
                                                    (joint-count nil
                                                     joint-count-p)
                                                    (joint-locations nil
                                                     joint-locations-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-joint-locations-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:is-active
                                %:joint-count
                                %:joint-locations)
                               ,pointer (:struct %:hand-joint-locations-ext))
       (setf %:type :type-hand-joint-locations-ext
             %:next ,next
             ,@(when is-active-p `(%:is-active ,is-active))
             ,@(when joint-count-p `(%:joint-count ,joint-count))
             ,@(when joint-locations-p `(%:joint-locations ,joint-locations)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-joint-locations-ext %struct-types%)
      ':type-hand-joint-locations-ext)

(defmacro with-hand-joint-velocities-ext ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (joint-count nil
                                                      joint-count-p)
                                                     (joint-velocities nil
                                                      joint-velocities-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-joint-velocities-ext))
     (cffi:with-foreign-slots ((%:type %:next %:joint-count %:joint-velocities)
                               ,pointer (:struct %:hand-joint-velocities-ext))
       (setf %:type :type-hand-joint-velocities-ext
             %:next ,next
             ,@(when joint-count-p `(%:joint-count ,joint-count))
             ,@(when joint-velocities-p `(%:joint-velocities ,joint-velocities)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-joint-velocities-ext %struct-types%)
      ':type-hand-joint-velocities-ext)

(defmacro with-system-face-tracking-properties-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (supports-face-tracking
                                                               nil
                                                               supports-face-tracking-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-face-tracking-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-face-tracking)
                               ,pointer (:struct %:system-face-tracking-properties-fb))
       (setf %:type :type-system-face-tracking-properties-fb
             %:next ,next
             ,@(when supports-face-tracking-p `(%:supports-face-tracking ,supports-face-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-face-tracking-properties-fb %struct-types%)
      ':type-system-face-tracking-properties-fb)

(defmacro with-face-tracker-create-info-fb ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (face-expression-set nil
                                                        face-expression-set-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:face-tracker-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:face-expression-set)
                               ,pointer (:struct %:face-tracker-create-info-fb))
       (setf %:type :type-face-tracker-create-info-fb
             %:next ,next
             ,@(when face-expression-set-p `(%:face-expression-set ,face-expression-set)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:face-tracker-create-info-fb %struct-types%)
      ':type-face-tracker-create-info-fb)

(defmacro with-face-expression-info-fb ((pointer &key %slots
                                                   (next '(cffi:null-pointer))
                                                   (time nil time-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:face-expression-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:time)
                               ,pointer (:struct %:face-expression-info-fb))
       (setf %:type :type-face-expression-info-fb
             %:next ,next
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:face-expression-info-fb %struct-types%)
      ':type-face-expression-info-fb)

(defmacro with-face-expression-status-fb ((pointer &key %slots
                                                     (is-valid nil is-valid-p)
                                                     (is-eye-following-blendshapes-valid
                                                      nil
                                                      is-eye-following-blendshapes-valid-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:face-expression-status-fb))
     (cffi:with-foreign-slots ((%:is-valid %:is-eye-following-blendshapes-valid)
                               ,pointer (:struct %:face-expression-status-fb))
       (setf ,@(when is-valid-p `(%:is-valid ,is-valid))
             ,@(when is-eye-following-blendshapes-valid-p `(%:is-eye-following-blendshapes-valid ,is-eye-following-blendshapes-valid)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-face-expression-weights-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (weight-count nil
                                                       weight-count-p)
                                                      (weights nil weights-p)
                                                      (confidence-count nil
                                                       confidence-count-p)
                                                      (confidences nil
                                                       confidences-p)
                                                      (status nil status-p)
                                                      (time nil time-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:face-expression-weights-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:weight-count
                                %:weights
                                %:confidence-count
                                %:confidences
                                %:status
                                %:time)
                               ,pointer (:struct %:face-expression-weights-fb))
       (setf %:type :type-face-expression-weights-fb
             %:next ,next
             ,@(when weight-count-p `(%:weight-count ,weight-count))
             ,@(when weights-p `(%:weights ,weights))
             ,@(when confidence-count-p `(%:confidence-count ,confidence-count))
             ,@(when confidences-p `(%:confidences ,confidences))
             ,@(when status-p `(%:status ,status))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:face-expression-weights-fb %struct-types%)
      ':type-face-expression-weights-fb)

(defmacro with-system-body-tracking-properties-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (supports-body-tracking
                                                               nil
                                                               supports-body-tracking-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-body-tracking-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-body-tracking)
                               ,pointer (:struct %:system-body-tracking-properties-fb))
       (setf %:type :type-system-body-tracking-properties-fb
             %:next ,next
             ,@(when supports-body-tracking-p `(%:supports-body-tracking ,supports-body-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-body-tracking-properties-fb %struct-types%)
      ':type-system-body-tracking-properties-fb)

(defmacro with-body-tracker-create-info-fb ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (body-joint-set nil
                                                        body-joint-set-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:body-tracker-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:body-joint-set)
                               ,pointer (:struct %:body-tracker-create-info-fb))
       (setf %:type :type-body-tracker-create-info-fb
             %:next ,next
             ,@(when body-joint-set-p `(%:body-joint-set ,body-joint-set)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:body-tracker-create-info-fb %struct-types%)
      ':type-body-tracker-create-info-fb)

(defmacro with-body-skeleton-joint-fb ((pointer &key %slots (joint nil joint-p)
                                                  (parent-joint nil
                                                   parent-joint-p)
                                                  (pose nil pose-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:body-skeleton-joint-fb))
     (cffi:with-foreign-slots ((%:joint %:parent-joint %:pose)
                               ,pointer (:struct %:body-skeleton-joint-fb))
       (setf ,@(when joint-p `(%:joint ,joint))
             ,@(when parent-joint-p `(%:parent-joint ,parent-joint))
             ,@(when pose-p `(%:pose ,pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-body-skeleton-fb ((pointer &key %slots
                                            (next '(cffi:null-pointer))
                                            (joint-count nil joint-count-p)
                                            (joints nil joints-p))
                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:body-skeleton-fb))
     (cffi:with-foreign-slots ((%:type %:next %:joint-count %:joints)
                               ,pointer (:struct %:body-skeleton-fb))
       (setf %:type :type-body-skeleton-fb
             %:next ,next
             ,@(when joint-count-p `(%:joint-count ,joint-count))
             ,@(when joints-p `(%:joints ,joints)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:body-skeleton-fb %struct-types%)
      ':type-body-skeleton-fb)

(defmacro with-body-joints-locate-info-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (base-space nil
                                                       base-space-p)
                                                      (time nil time-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:body-joints-locate-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:base-space %:time)
                               ,pointer (:struct %:body-joints-locate-info-fb))
       (setf %:type :type-body-joints-locate-info-fb
             %:next ,next
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:body-joints-locate-info-fb %struct-types%)
      ':type-body-joints-locate-info-fb)

(defmacro with-body-joint-location-fb ((pointer &key %slots
                                                  (location-flags nil
                                                   location-flags-p)
                                                  (pose nil pose-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:body-joint-location-fb))
     (cffi:with-foreign-slots ((%:location-flags %:pose)
                               ,pointer (:struct %:body-joint-location-fb))
       (setf ,@(when location-flags-p `(%:location-flags ,location-flags))
             ,@(when pose-p `(%:pose ,pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-body-joint-locations-fb ((pointer &key %slots
                                                   (next '(cffi:null-pointer))
                                                   (is-active nil is-active-p)
                                                   (confidence nil
                                                    confidence-p)
                                                   (joint-count nil
                                                    joint-count-p)
                                                   (joint-locations nil
                                                    joint-locations-p)
                                                   (skeleton-changed-count nil
                                                    skeleton-changed-count-p)
                                                   (time nil time-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:body-joint-locations-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:is-active
                                %:confidence
                                %:joint-count
                                %:joint-locations
                                %:skeleton-changed-count
                                %:time)
                               ,pointer (:struct %:body-joint-locations-fb))
       (setf %:type :type-body-joint-locations-fb
             %:next ,next
             ,@(when is-active-p `(%:is-active ,is-active))
             ,@(when confidence-p `(%:confidence ,confidence))
             ,@(when joint-count-p `(%:joint-count ,joint-count))
             ,@(when joint-locations-p `(%:joint-locations ,joint-locations))
             ,@(when skeleton-changed-count-p `(%:skeleton-changed-count ,skeleton-changed-count))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:body-joint-locations-fb %struct-types%)
      ':type-body-joint-locations-fb)

(defmacro with-system-eye-tracking-properties-fb ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (supports-eye-tracking
                                                              nil
                                                              supports-eye-tracking-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-eye-tracking-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-eye-tracking)
                               ,pointer (:struct %:system-eye-tracking-properties-fb))
       (setf %:type :type-system-eye-tracking-properties-fb
             %:next ,next
             ,@(when supports-eye-tracking-p `(%:supports-eye-tracking ,supports-eye-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-eye-tracking-properties-fb %struct-types%)
      ':type-system-eye-tracking-properties-fb)

(defmacro with-eye-tracker-create-info-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer)))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:eye-tracker-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:eye-tracker-create-info-fb))
       (setf %:type :type-eye-tracker-create-info-fb
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:eye-tracker-create-info-fb %struct-types%)
      ':type-eye-tracker-create-info-fb)

(defmacro with-eye-gazes-info-fb ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (base-space nil base-space-p)
                                             (time nil time-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:eye-gazes-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:base-space %:time)
                               ,pointer (:struct %:eye-gazes-info-fb))
       (setf %:type :type-eye-gazes-info-fb
             %:next ,next
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:eye-gazes-info-fb %struct-types%)
      ':type-eye-gazes-info-fb)

(defmacro with-eye-gaze-fb ((pointer &key %slots (is-valid nil is-valid-p)
                                       (gaze-pose nil gaze-pose-p)
                                       (gaze-confidence nil gaze-confidence-p))
                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:eye-gaze-fb))
     (cffi:with-foreign-slots ((%:is-valid %:gaze-pose %:gaze-confidence)
                               ,pointer (:struct %:eye-gaze-fb))
       (setf ,@(when is-valid-p `(%:is-valid ,is-valid))
             ,@(when gaze-pose-p `(%:gaze-pose ,gaze-pose))
             ,@(when gaze-confidence-p `(%:gaze-confidence ,gaze-confidence)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-eye-gazes-fb ((pointer &key %slots (next '(cffi:null-pointer))
                                        (gaze nil gaze-p) (time nil time-p))
                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:eye-gazes-fb))
     (cffi:with-foreign-slots ((%:type %:next %:gaze %:time)
                               ,pointer (:struct %:eye-gazes-fb))
       (setf %:type :type-eye-gazes-fb
             %:next ,next
             ,@(when gaze-p `(%:gaze ,gaze))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:eye-gazes-fb %struct-types%)
      ':type-eye-gazes-fb)

(defmacro with-hand-joints-motion-range-info-ext ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (hand-joints-motion-range
                                                              nil
                                                              hand-joints-motion-range-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-joints-motion-range-info-ext))
     (cffi:with-foreign-slots ((%:type %:next %:hand-joints-motion-range)
                               ,pointer (:struct %:hand-joints-motion-range-info-ext))
       (setf %:type :type-hand-joints-motion-range-info-ext
             %:next ,next
             ,@(when hand-joints-motion-range-p `(%:hand-joints-motion-range ,hand-joints-motion-range)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-joints-motion-range-info-ext %struct-types%)
      ':type-hand-joints-motion-range-info-ext)

(defmacro with-hand-mesh-space-create-info-msft ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (hand-pose-type nil
                                                             hand-pose-type-p)
                                                            (pose-in-hand-mesh-space
                                                             nil
                                                             pose-in-hand-mesh-space-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-mesh-space-create-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:hand-pose-type
                                %:pose-in-hand-mesh-space)
                               ,pointer (:struct %:hand-mesh-space-create-info-msft))
       (setf %:type :type-hand-mesh-space-create-info-msft
             %:next ,next
             ,@(when hand-pose-type-p `(%:hand-pose-type ,hand-pose-type))
             ,@(when pose-in-hand-mesh-space-p `(%:pose-in-hand-mesh-space ,pose-in-hand-mesh-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-mesh-space-create-info-msft %struct-types%)
      ':type-hand-mesh-space-create-info-msft)

(defmacro with-hand-mesh-update-info-msft ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (time nil time-p)
                                                      (hand-pose-type nil
                                                       hand-pose-type-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-mesh-update-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:time %:hand-pose-type)
                               ,pointer (:struct %:hand-mesh-update-info-msft))
       (setf %:type :type-hand-mesh-update-info-msft
             %:next ,next
             ,@(when time-p `(%:time ,time))
             ,@(when hand-pose-type-p `(%:hand-pose-type ,hand-pose-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-mesh-update-info-msft %struct-types%)
      ':type-hand-mesh-update-info-msft)

(defmacro with-hand-mesh-msft ((pointer &key %slots (next '(cffi:null-pointer))
                                          (is-active nil is-active-p)
                                          (index-buffer-changed nil
                                           index-buffer-changed-p)
                                          (vertex-buffer-changed nil
                                           vertex-buffer-changed-p)
                                          (index-buffer nil index-buffer-p)
                                          (vertex-buffer nil vertex-buffer-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-mesh-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:is-active
                                %:index-buffer-changed
                                %:vertex-buffer-changed
                                %:index-buffer
                                %:vertex-buffer)
                               ,pointer (:struct %:hand-mesh-msft))
       (setf %:type :type-hand-mesh-msft
             %:next ,next
             ,@(when is-active-p `(%:is-active ,is-active))
             ,@(when index-buffer-changed-p `(%:index-buffer-changed ,index-buffer-changed))
             ,@(when vertex-buffer-changed-p `(%:vertex-buffer-changed ,vertex-buffer-changed))
             ,@(when index-buffer-p `(%:index-buffer ,index-buffer))
             ,@(when vertex-buffer-p `(%:vertex-buffer ,vertex-buffer)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-mesh-msft %struct-types%)
      ':type-hand-mesh-msft)

(defmacro with-hand-mesh-index-buffer-msft ((pointer &key %slots
                                                       (index-buffer-key nil
                                                        index-buffer-key-p)
                                                       (index-capacity-input
                                                        nil
                                                        index-capacity-input-p)
                                                       (index-count-output nil
                                                        index-count-output-p)
                                                       (indices nil indices-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-mesh-index-buffer-msft))
     (cffi:with-foreign-slots ((%:index-buffer-key
                                %:index-capacity-input
                                %:index-count-output
                                %:indices)
                               ,pointer (:struct %:hand-mesh-index-buffer-msft))
       (setf ,@(when index-buffer-key-p `(%:index-buffer-key ,index-buffer-key))
             ,@(when index-capacity-input-p `(%:index-capacity-input ,index-capacity-input))
             ,@(when index-count-output-p `(%:index-count-output ,index-count-output))
             ,@(when indices-p `(%:indices ,indices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-hand-mesh-vertex-buffer-msft ((pointer &key %slots
                                                        (vertex-update-time nil
                                                         vertex-update-time-p)
                                                        (vertex-capacity-input
                                                         nil
                                                         vertex-capacity-input-p)
                                                        (vertex-count-output
                                                         nil
                                                         vertex-count-output-p)
                                                        (vertices nil
                                                         vertices-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-mesh-vertex-buffer-msft))
     (cffi:with-foreign-slots ((%:vertex-update-time
                                %:vertex-capacity-input
                                %:vertex-count-output
                                %:vertices)
                               ,pointer (:struct %:hand-mesh-vertex-buffer-msft))
       (setf ,@(when vertex-update-time-p `(%:vertex-update-time ,vertex-update-time))
             ,@(when vertex-capacity-input-p `(%:vertex-capacity-input ,vertex-capacity-input))
             ,@(when vertex-count-output-p `(%:vertex-count-output ,vertex-count-output))
             ,@(when vertices-p `(%:vertices ,vertices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-hand-mesh-vertex-msft ((pointer &key %slots
                                                 (position nil position-p)
                                                 (normal nil normal-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-mesh-vertex-msft))
     (cffi:with-foreign-slots ((%:position %:normal)
                               ,pointer (:struct %:hand-mesh-vertex-msft))
       (setf ,@(when position-p `(%:position ,position))
             ,@(when normal-p `(%:normal ,normal)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-system-hand-tracking-mesh-properties-msft ((pointer &key %slots
                                                                     (next
                                                                      '(cffi:null-pointer))
                                                                     (supports-hand-tracking-mesh
                                                                      nil
                                                                      supports-hand-tracking-mesh-p)
                                                                     (max-hand-mesh-index-count
                                                                      nil
                                                                      max-hand-mesh-index-count-p)
                                                                     (max-hand-mesh-vertex-count
                                                                      nil
                                                                      max-hand-mesh-vertex-count-p))
                                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-hand-tracking-mesh-properties-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:supports-hand-tracking-mesh
                                %:max-hand-mesh-index-count
                                %:max-hand-mesh-vertex-count)
                               ,pointer (:struct %:system-hand-tracking-mesh-properties-msft))
       (setf %:type :type-system-hand-tracking-mesh-properties-msft
             %:next ,next
             ,@(when supports-hand-tracking-mesh-p `(%:supports-hand-tracking-mesh ,supports-hand-tracking-mesh))
             ,@(when max-hand-mesh-index-count-p `(%:max-hand-mesh-index-count ,max-hand-mesh-index-count))
             ,@(when max-hand-mesh-vertex-count-p `(%:max-hand-mesh-vertex-count ,max-hand-mesh-vertex-count)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-hand-tracking-mesh-properties-msft %struct-types%)
      ':type-system-hand-tracking-mesh-properties-msft)

(defmacro with-hand-pose-type-info-msft ((pointer &key %slots
                                                    (next '(cffi:null-pointer))
                                                    (hand-pose-type nil
                                                     hand-pose-type-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-pose-type-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:hand-pose-type)
                               ,pointer (:struct %:hand-pose-type-info-msft))
       (setf %:type :type-hand-pose-type-info-msft
             %:next ,next
             ,@(when hand-pose-type-p `(%:hand-pose-type ,hand-pose-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-pose-type-info-msft %struct-types%)
      ':type-hand-pose-type-info-msft)

(defmacro with-secondary-view-configuration-session-begin-info-msft ((pointer &key
                                                                                %slots
                                                                                (next
                                                                                 '(cffi:null-pointer))
                                                                                (view-configuration-count
                                                                                 nil
                                                                                 view-configuration-count-p)
                                                                                (enabled-view-configuration-types
                                                                                 nil
                                                                                 enabled-view-configuration-types-p))
                                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:secondary-view-configuration-session-begin-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-count
                                %:enabled-view-configuration-types)
                               ,pointer (:struct %:secondary-view-configuration-session-begin-info-msft))
       (setf %:type :type-secondary-view-configuration-session-begin-info-msft
             %:next ,next
             ,@(when view-configuration-count-p `(%:view-configuration-count ,view-configuration-count))
             ,@(when enabled-view-configuration-types-p `(%:enabled-view-configuration-types ,enabled-view-configuration-types)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:secondary-view-configuration-session-begin-info-msft %struct-types%)
      ':type-secondary-view-configuration-session-begin-info-msft)

(defmacro with-secondary-view-configuration-state-msft ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (view-configuration-type
                                                                    nil
                                                                    view-configuration-type-p)
                                                                   (active nil
                                                                    active-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:secondary-view-configuration-state-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-type
                                %:active)
                               ,pointer (:struct %:secondary-view-configuration-state-msft))
       (setf %:type :type-secondary-view-configuration-state-msft
             %:next ,next
             ,@(when view-configuration-type-p `(%:view-configuration-type ,view-configuration-type))
             ,@(when active-p `(%:active ,active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:secondary-view-configuration-state-msft %struct-types%)
      ':type-secondary-view-configuration-state-msft)

(defmacro with-secondary-view-configuration-frame-state-msft ((pointer &key
                                                                         %slots
                                                                         (next
                                                                          '(cffi:null-pointer))
                                                                         (view-configuration-count
                                                                          nil
                                                                          view-configuration-count-p)
                                                                         (view-configuration-states
                                                                          nil
                                                                          view-configuration-states-p))
                                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:secondary-view-configuration-frame-state-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-count
                                %:view-configuration-states)
                               ,pointer (:struct %:secondary-view-configuration-frame-state-msft))
       (setf %:type :type-secondary-view-configuration-frame-state-msft
             %:next ,next
             ,@(when view-configuration-count-p `(%:view-configuration-count ,view-configuration-count))
             ,@(when view-configuration-states-p `(%:view-configuration-states ,view-configuration-states)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:secondary-view-configuration-frame-state-msft %struct-types%)
      ':type-secondary-view-configuration-frame-state-msft)

(defmacro with-secondary-view-configuration-frame-end-info-msft ((pointer &key
                                                                            %slots
                                                                            (next
                                                                             '(cffi:null-pointer))
                                                                            (view-configuration-count
                                                                             nil
                                                                             view-configuration-count-p)
                                                                            (view-configuration-layers-info
                                                                             nil
                                                                             view-configuration-layers-info-p))
                                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:secondary-view-configuration-frame-end-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-count
                                %:view-configuration-layers-info)
                               ,pointer (:struct %:secondary-view-configuration-frame-end-info-msft))
       (setf %:type :type-secondary-view-configuration-frame-end-info-msft
             %:next ,next
             ,@(when view-configuration-count-p `(%:view-configuration-count ,view-configuration-count))
             ,@(when view-configuration-layers-info-p `(%:view-configuration-layers-info ,view-configuration-layers-info)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:secondary-view-configuration-frame-end-info-msft %struct-types%)
      ':type-secondary-view-configuration-frame-end-info-msft)

(defmacro with-secondary-view-configuration-layer-info-msft ((pointer &key
                                                                        %slots
                                                                        (next
                                                                         '(cffi:null-pointer))
                                                                        (view-configuration-type
                                                                         nil
                                                                         view-configuration-type-p)
                                                                        (environment-blend-mode
                                                                         nil
                                                                         environment-blend-mode-p)
                                                                        (layer-count
                                                                         nil
                                                                         layer-count-p)
                                                                        (layers
                                                                         nil
                                                                         layers-p))
                                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:secondary-view-configuration-layer-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:view-configuration-type
                                %:environment-blend-mode
                                %:layer-count
                                %:layers)
                               ,pointer (:struct %:secondary-view-configuration-layer-info-msft))
       (setf %:type :type-secondary-view-configuration-layer-info-msft
             %:next ,next
             ,@(when view-configuration-type-p `(%:view-configuration-type ,view-configuration-type))
             ,@(when environment-blend-mode-p `(%:environment-blend-mode ,environment-blend-mode))
             ,@(when layer-count-p `(%:layer-count ,layer-count))
             ,@(when layers-p `(%:layers ,layers)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:secondary-view-configuration-layer-info-msft %struct-types%)
      ':type-secondary-view-configuration-layer-info-msft)

(defmacro with-secondary-view-configuration-swapchain-create-info-msft ((pointer &key
                                                                                   %slots
                                                                                   (next
                                                                                    '(cffi:null-pointer))
                                                                                   (view-configuration-type
                                                                                    nil
                                                                                    view-configuration-type-p))
                                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:secondary-view-configuration-swapchain-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:view-configuration-type)
                               ,pointer (:struct %:secondary-view-configuration-swapchain-create-info-msft))
       (setf %:type :type-secondary-view-configuration-swapchain-create-info-msft
             %:next ,next
             ,@(when view-configuration-type-p `(%:view-configuration-type ,view-configuration-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:secondary-view-configuration-swapchain-create-info-msft %struct-types%)
      ':type-secondary-view-configuration-swapchain-create-info-msft)

(defmacro with-holographic-window-attachment-msft ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (holographic-space
                                                               nil
                                                               holographic-space-p)
                                                              (core-window nil
                                                               core-window-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:holographic-window-attachment-msft))
     (cffi:with-foreign-slots ((%:type %:next %:holographic-space %:core-window)
                               ,pointer (:struct %:holographic-window-attachment-msft))
       (setf %:type :type-holographic-window-attachment-msft
             %:next ,next
             ,@(when holographic-space-p `(%:holographic-space ,holographic-space))
             ,@(when core-window-p `(%:core-window ,core-window)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:holographic-window-attachment-msft %struct-types%)
      ':type-holographic-window-attachment-msft)

(defmacro with-android-surface-swapchain-create-info-fb ((pointer &key %slots
                                                                    (next
                                                                     '(cffi:null-pointer))
                                                                    (create-flags
                                                                     nil
                                                                     create-flags-p))
                                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:android-surface-swapchain-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:create-flags)
                               ,pointer (:struct %:android-surface-swapchain-create-info-fb))
       (setf %:type :type-android-surface-swapchain-create-info-fb
             %:next ,next
             ,@(when create-flags-p `(%:create-flags ,create-flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:android-surface-swapchain-create-info-fb %struct-types%)
      ':type-android-surface-swapchain-create-info-fb)

(defmacro with-swapchain-state-android-surface-dimensions-fb ((pointer &key
                                                                         %slots
                                                                         (next
                                                                          '(cffi:null-pointer))
                                                                         (width
                                                                          nil
                                                                          width-p)
                                                                         (height
                                                                          nil
                                                                          height-p))
                                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-state-android-surface-dimensions-fb))
     (cffi:with-foreign-slots ((%:type %:next %:width %:height)
                               ,pointer (:struct %:swapchain-state-android-surface-dimensions-fb))
       (setf %:type :type-swapchain-state-android-surface-dimensions-fb
             %:next ,next
             ,@(when width-p `(%:width ,width))
             ,@(when height-p `(%:height ,height)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-state-android-surface-dimensions-fb %struct-types%)
      ':type-swapchain-state-android-surface-dimensions-fb)

(defmacro with-swapchain-state-sampler-opengl-es-fb ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (min-filter nil
                                                                 min-filter-p)
                                                                (mag-filter nil
                                                                 mag-filter-p)
                                                                (wrap-mode-s
                                                                 nil
                                                                 wrap-mode-s-p)
                                                                (wrap-mode-t
                                                                 nil
                                                                 wrap-mode-t-p)
                                                                (swizzle-red
                                                                 nil
                                                                 swizzle-red-p)
                                                                (swizzle-green
                                                                 nil
                                                                 swizzle-green-p)
                                                                (swizzle-blue
                                                                 nil
                                                                 swizzle-blue-p)
                                                                (swizzle-alpha
                                                                 nil
                                                                 swizzle-alpha-p)
                                                                (max-anisotropy
                                                                 nil
                                                                 max-anisotropy-p)
                                                                (border-color
                                                                 nil
                                                                 border-color-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-state-sampler-opengl-es-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:min-filter
                                %:mag-filter
                                %:wrap-mode-s
                                %:wrap-mode-t
                                %:swizzle-red
                                %:swizzle-green
                                %:swizzle-blue
                                %:swizzle-alpha
                                %:max-anisotropy
                                %:border-color)
                               ,pointer (:struct %:swapchain-state-sampler-opengl-es-fb))
       (setf %:type :type-swapchain-state-sampler-opengl-es-fb
             %:next ,next
             ,@(when min-filter-p `(%:min-filter ,min-filter))
             ,@(when mag-filter-p `(%:mag-filter ,mag-filter))
             ,@(when wrap-mode-s-p `(%:wrap-mode-s ,wrap-mode-s))
             ,@(when wrap-mode-t-p `(%:wrap-mode-t ,wrap-mode-t))
             ,@(when swizzle-red-p `(%:swizzle-red ,swizzle-red))
             ,@(when swizzle-green-p `(%:swizzle-green ,swizzle-green))
             ,@(when swizzle-blue-p `(%:swizzle-blue ,swizzle-blue))
             ,@(when swizzle-alpha-p `(%:swizzle-alpha ,swizzle-alpha))
             ,@(when max-anisotropy-p `(%:max-anisotropy ,max-anisotropy))
             ,@(when border-color-p `(%:border-color ,border-color)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-state-sampler-opengl-es-fb %struct-types%)
      ':type-swapchain-state-sampler-opengl-es-fb)

(defmacro with-swapchain-state-sampler-vulkan-fb ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (min-filter nil
                                                              min-filter-p)
                                                             (mag-filter nil
                                                              mag-filter-p)
                                                             (mipmap-mode nil
                                                              mipmap-mode-p)
                                                             (wrap-mode-s nil
                                                              wrap-mode-s-p)
                                                             (wrap-mode-t nil
                                                              wrap-mode-t-p)
                                                             (swizzle-red nil
                                                              swizzle-red-p)
                                                             (swizzle-green nil
                                                              swizzle-green-p)
                                                             (swizzle-blue nil
                                                              swizzle-blue-p)
                                                             (swizzle-alpha nil
                                                              swizzle-alpha-p)
                                                             (max-anisotropy
                                                              nil
                                                              max-anisotropy-p)
                                                             (border-color nil
                                                              border-color-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-state-sampler-vulkan-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:min-filter
                                %:mag-filter
                                %:mipmap-mode
                                %:wrap-mode-s
                                %:wrap-mode-t
                                %:swizzle-red
                                %:swizzle-green
                                %:swizzle-blue
                                %:swizzle-alpha
                                %:max-anisotropy
                                %:border-color)
                               ,pointer (:struct %:swapchain-state-sampler-vulkan-fb))
       (setf %:type :type-swapchain-state-sampler-vulkan-fb
             %:next ,next
             ,@(when min-filter-p `(%:min-filter ,min-filter))
             ,@(when mag-filter-p `(%:mag-filter ,mag-filter))
             ,@(when mipmap-mode-p `(%:mipmap-mode ,mipmap-mode))
             ,@(when wrap-mode-s-p `(%:wrap-mode-s ,wrap-mode-s))
             ,@(when wrap-mode-t-p `(%:wrap-mode-t ,wrap-mode-t))
             ,@(when swizzle-red-p `(%:swizzle-red ,swizzle-red))
             ,@(when swizzle-green-p `(%:swizzle-green ,swizzle-green))
             ,@(when swizzle-blue-p `(%:swizzle-blue ,swizzle-blue))
             ,@(when swizzle-alpha-p `(%:swizzle-alpha ,swizzle-alpha))
             ,@(when max-anisotropy-p `(%:max-anisotropy ,max-anisotropy))
             ,@(when border-color-p `(%:border-color ,border-color)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-state-sampler-vulkan-fb %struct-types%)
      ':type-swapchain-state-sampler-vulkan-fb)

(defmacro with-composition-layer-secure-content-fb ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (flags nil
                                                                flags-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-secure-content-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:composition-layer-secure-content-fb))
       (setf %:type :type-composition-layer-secure-content-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-secure-content-fb %struct-types%)
      ':type-composition-layer-secure-content-fb)

(defmacro with-loader-init-info-android-khr ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer))
                                                        (application-vm nil
                                                         application-vm-p)
                                                        (application-context
                                                         nil
                                                         application-context-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:loader-init-info-android-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:application-vm
                                %:application-context)
                               ,pointer (:struct %:loader-init-info-android-khr))
       (setf %:type :type-loader-init-info-android-khr
             %:next ,next
             ,@(when application-vm-p `(%:application-vm ,application-vm))
             ,@(when application-context-p `(%:application-context ,application-context)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:loader-init-info-android-khr %struct-types%)
      ':type-loader-init-info-android-khr)

(defmacro with-composition-layer-equirect-2-khr ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (layer-flags nil
                                                             layer-flags-p)
                                                            (space nil space-p)
                                                            (eye-visibility nil
                                                             eye-visibility-p)
                                                            (sub-image nil
                                                             sub-image-p)
                                                            (pose nil pose-p)
                                                            (radius nil
                                                             radius-p)
                                                            (central-horizontal-angle
                                                             nil
                                                             central-horizontal-angle-p)
                                                            (upper-vertical-angle
                                                             nil
                                                             upper-vertical-angle-p)
                                                            (lower-vertical-angle
                                                             nil
                                                             lower-vertical-angle-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-equirect-2-khr))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:eye-visibility
                                %:sub-image
                                %:pose
                                %:radius
                                %:central-horizontal-angle
                                %:upper-vertical-angle
                                %:lower-vertical-angle)
                               ,pointer (:struct %:composition-layer-equirect-2-khr))
       (setf %:type :type-composition-layer-equirect2-khr
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when eye-visibility-p `(%:eye-visibility ,eye-visibility))
             ,@(when sub-image-p `(%:sub-image ,sub-image))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when radius-p `(%:radius ,radius))
             ,@(when central-horizontal-angle-p `(%:central-horizontal-angle ,central-horizontal-angle))
             ,@(when upper-vertical-angle-p `(%:upper-vertical-angle ,upper-vertical-angle))
             ,@(when lower-vertical-angle-p `(%:lower-vertical-angle ,lower-vertical-angle)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-equirect-2-khr %struct-types%)
      ':type-composition-layer-equirect2-khr)

(defmacro with-composition-layer-color-scale-bias-khr ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (color-scale
                                                                   nil
                                                                   color-scale-p)
                                                                  (color-bias
                                                                   nil
                                                                   color-bias-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-color-scale-bias-khr))
     (cffi:with-foreign-slots ((%:type %:next %:color-scale %:color-bias)
                               ,pointer (:struct %:composition-layer-color-scale-bias-khr))
       (setf %:type :type-composition-layer-color-scale-bias-khr
             %:next ,next
             ,@(when color-scale-p `(%:color-scale ,color-scale))
             ,@(when color-bias-p `(%:color-bias ,color-bias)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-color-scale-bias-khr %struct-types%)
      ':type-composition-layer-color-scale-bias-khr)

(defmacro with-controller-model-key-state-msft ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (model-key nil
                                                            model-key-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:controller-model-key-state-msft))
     (cffi:with-foreign-slots ((%:type %:next %:model-key)
                               ,pointer (:struct %:controller-model-key-state-msft))
       (setf %:type :type-controller-model-key-state-msft
             %:next ,next
             ,@(when model-key-p `(%:model-key ,model-key)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:controller-model-key-state-msft %struct-types%)
      ':type-controller-model-key-state-msft)

(defmacro with-controller-model-node-properties-msft ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (parent-node-name
                                                                  nil
                                                                  parent-node-name-p)
                                                                 (node-name nil
                                                                  node-name-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:controller-model-node-properties-msft))
     (cffi:with-foreign-slots ((%:type %:next %:parent-node-name %:node-name)
                               ,pointer (:struct %:controller-model-node-properties-msft))
       (setf %:type :type-controller-model-node-properties-msft
             %:next ,next)
       (if (and ,parent-node-name-p (not ,parent-node-name))
           (setf %:parent-node-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,parent-node-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:controller-model-node-properties-msft)
             '%:parent-node-name)
            %:+max-controller-model-node-name-size-msft+ :encoding :utf-8))
       (if (and ,node-name-p (not ,node-name))
           (setf %:node-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,node-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:controller-model-node-properties-msft) '%:node-name)
            %:+max-controller-model-node-name-size-msft+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:controller-model-node-properties-msft %struct-types%)
      ':type-controller-model-node-properties-msft)

(defmacro with-controller-model-properties-msft ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (node-capacity-input
                                                             nil
                                                             node-capacity-input-p)
                                                            (node-count-output
                                                             nil
                                                             node-count-output-p)
                                                            (node-properties
                                                             nil
                                                             node-properties-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:controller-model-properties-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:node-capacity-input
                                %:node-count-output
                                %:node-properties)
                               ,pointer (:struct %:controller-model-properties-msft))
       (setf %:type :type-controller-model-properties-msft
             %:next ,next
             ,@(when node-capacity-input-p `(%:node-capacity-input ,node-capacity-input))
             ,@(when node-count-output-p `(%:node-count-output ,node-count-output))
             ,@(when node-properties-p `(%:node-properties ,node-properties)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:controller-model-properties-msft %struct-types%)
      ':type-controller-model-properties-msft)

(defmacro with-controller-model-node-state-msft ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (node-pose nil
                                                             node-pose-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:controller-model-node-state-msft))
     (cffi:with-foreign-slots ((%:type %:next %:node-pose)
                               ,pointer (:struct %:controller-model-node-state-msft))
       (setf %:type :type-controller-model-node-state-msft
             %:next ,next
             ,@(when node-pose-p `(%:node-pose ,node-pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:controller-model-node-state-msft %struct-types%)
      ':type-controller-model-node-state-msft)

(defmacro with-controller-model-state-msft ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (node-capacity-input nil
                                                        node-capacity-input-p)
                                                       (node-count-output nil
                                                        node-count-output-p)
                                                       (node-states nil
                                                        node-states-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:controller-model-state-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:node-capacity-input
                                %:node-count-output
                                %:node-states)
                               ,pointer (:struct %:controller-model-state-msft))
       (setf %:type :type-controller-model-state-msft
             %:next ,next
             ,@(when node-capacity-input-p `(%:node-capacity-input ,node-capacity-input))
             ,@(when node-count-output-p `(%:node-count-output ,node-count-output))
             ,@(when node-states-p `(%:node-states ,node-states)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:controller-model-state-msft %struct-types%)
      ':type-controller-model-state-msft)

(defmacro with-uuid-msft ((pointer &key %slots (bytes nil bytes-p)) &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:uuid-msft))
     (cffi:with-foreign-slots ((%:bytes)
                               ,pointer (:struct %:uuid-msft))
       (setf ,@(when bytes-p `(%:bytes ,bytes)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-observer-create-info-msft ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer)))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-observer-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:scene-observer-create-info-msft))
       (setf %:type :type-scene-observer-create-info-msft
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-observer-create-info-msft %struct-types%)
      ':type-scene-observer-create-info-msft)

(defmacro with-scene-create-info-msft ((pointer &key %slots
                                                  (next '(cffi:null-pointer)))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-create-info-msft))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:scene-create-info-msft))
       (setf %:type :type-scene-create-info-msft
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-create-info-msft %struct-types%)
      ':type-scene-create-info-msft)

(defmacro with-new-scene-compute-info-msft ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (requested-feature-count
                                                        nil
                                                        requested-feature-count-p)
                                                       (requested-features nil
                                                        requested-features-p)
                                                       (consistency nil
                                                        consistency-p)
                                                       (bounds nil bounds-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:new-scene-compute-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:requested-feature-count
                                %:requested-features
                                %:consistency
                                %:bounds)
                               ,pointer (:struct %:new-scene-compute-info-msft))
       (setf %:type :type-new-scene-compute-info-msft
             %:next ,next
             ,@(when requested-feature-count-p `(%:requested-feature-count ,requested-feature-count))
             ,@(when requested-features-p `(%:requested-features ,requested-features))
             ,@(when consistency-p `(%:consistency ,consistency))
             ,@(when bounds-p `(%:bounds ,bounds)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:new-scene-compute-info-msft %struct-types%)
      ':type-new-scene-compute-info-msft)

(defmacro with-visual-mesh-compute-lod-info-msft ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (lod nil lod-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:visual-mesh-compute-lod-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:lod)
                               ,pointer (:struct %:visual-mesh-compute-lod-info-msft))
       (setf %:type :type-visual-mesh-compute-lod-info-msft
             %:next ,next
             ,@(when lod-p `(%:lod ,lod)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:visual-mesh-compute-lod-info-msft %struct-types%)
      ':type-visual-mesh-compute-lod-info-msft)

(defmacro with-scene-sphere-bound-msft ((pointer &key %slots
                                                   (center nil center-p)
                                                   (radius nil radius-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-sphere-bound-msft))
     (cffi:with-foreign-slots ((%:center %:radius)
                               ,pointer (:struct %:scene-sphere-bound-msft))
       (setf ,@(when center-p `(%:center ,center))
             ,@(when radius-p `(%:radius ,radius)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-oriented-box-bound-msft ((pointer &key %slots
                                                         (pose nil pose-p)
                                                         (extents nil
                                                          extents-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-oriented-box-bound-msft))
     (cffi:with-foreign-slots ((%:pose %:extents)
                               ,pointer (:struct %:scene-oriented-box-bound-msft))
       (setf ,@(when pose-p `(%:pose ,pose))
             ,@(when extents-p `(%:extents ,extents)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-frustum-bound-msft ((pointer &key %slots (pose nil pose-p)
                                                    (fov nil fov-p)
                                                    (far-distance nil
                                                     far-distance-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-frustum-bound-msft))
     (cffi:with-foreign-slots ((%:pose %:fov %:far-distance)
                               ,pointer (:struct %:scene-frustum-bound-msft))
       (setf ,@(when pose-p `(%:pose ,pose))
             ,@(when fov-p `(%:fov ,fov))
             ,@(when far-distance-p `(%:far-distance ,far-distance)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-bounds-msft ((pointer &key %slots (space nil space-p)
                                             (time nil time-p)
                                             (sphere-count nil sphere-count-p)
                                             (spheres nil spheres-p)
                                             (box-count nil box-count-p)
                                             (boxes nil boxes-p)
                                             (frustum-count nil
                                              frustum-count-p)
                                             (frustums nil frustums-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-bounds-msft))
     (cffi:with-foreign-slots ((%:space
                                %:time
                                %:sphere-count
                                %:spheres
                                %:box-count
                                %:boxes
                                %:frustum-count
                                %:frustums)
                               ,pointer (:struct %:scene-bounds-msft))
       (setf ,@(when space-p `(%:space ,space))
             ,@(when time-p `(%:time ,time))
             ,@(when sphere-count-p `(%:sphere-count ,sphere-count))
             ,@(when spheres-p `(%:spheres ,spheres))
             ,@(when box-count-p `(%:box-count ,box-count))
             ,@(when boxes-p `(%:boxes ,boxes))
             ,@(when frustum-count-p `(%:frustum-count ,frustum-count))
             ,@(when frustums-p `(%:frustums ,frustums)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-component-msft ((pointer &key %slots
                                                (component-type nil
                                                 component-type-p)
                                                (id nil id-p)
                                                (parent-id nil parent-id-p)
                                                (update-time nil update-time-p))
                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-component-msft))
     (cffi:with-foreign-slots ((%:component-type %:id %:parent-id %:update-time)
                               ,pointer (:struct %:scene-component-msft))
       (setf ,@(when component-type-p `(%:component-type ,component-type))
             ,@(when id-p `(%:id ,id))
             ,@(when parent-id-p `(%:parent-id ,parent-id))
             ,@(when update-time-p `(%:update-time ,update-time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-components-msft ((pointer &key %slots
                                                 (next '(cffi:null-pointer))
                                                 (component-capacity-input nil
                                                  component-capacity-input-p)
                                                 (component-count-output nil
                                                  component-count-output-p)
                                                 (components nil components-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-components-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:component-capacity-input
                                %:component-count-output
                                %:components)
                               ,pointer (:struct %:scene-components-msft))
       (setf %:type :type-scene-components-msft
             %:next ,next
             ,@(when component-capacity-input-p `(%:component-capacity-input ,component-capacity-input))
             ,@(when component-count-output-p `(%:component-count-output ,component-count-output))
             ,@(when components-p `(%:components ,components)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-components-msft %struct-types%)
      ':type-scene-components-msft)

(defmacro with-scene-components-get-info-msft ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (component-type nil
                                                           component-type-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-components-get-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:component-type)
                               ,pointer (:struct %:scene-components-get-info-msft))
       (setf %:type :type-scene-components-get-info-msft
             %:next ,next
             ,@(when component-type-p `(%:component-type ,component-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-components-get-info-msft %struct-types%)
      ':type-scene-components-get-info-msft)

(defmacro with-scene-component-location-msft ((pointer &key %slots
                                                         (flags nil flags-p)
                                                         (pose nil pose-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-component-location-msft))
     (cffi:with-foreign-slots ((%:flags %:pose)
                               ,pointer (:struct %:scene-component-location-msft))
       (setf ,@(when flags-p `(%:flags ,flags))
             ,@(when pose-p `(%:pose ,pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-component-locations-msft ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (location-count nil
                                                           location-count-p)
                                                          (locations nil
                                                           locations-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-component-locations-msft))
     (cffi:with-foreign-slots ((%:type %:next %:location-count %:locations)
                               ,pointer (:struct %:scene-component-locations-msft))
       (setf %:type :type-scene-component-locations-msft
             %:next ,next
             ,@(when location-count-p `(%:location-count ,location-count))
             ,@(when locations-p `(%:locations ,locations)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-component-locations-msft %struct-types%)
      ':type-scene-component-locations-msft)

(defmacro with-scene-components-locate-info-msft ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (base-space nil
                                                              base-space-p)
                                                             (time nil time-p)
                                                             (component-id-count
                                                              nil
                                                              component-id-count-p)
                                                             (component-ids nil
                                                              component-ids-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-components-locate-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:base-space
                                %:time
                                %:component-id-count
                                %:component-ids)
                               ,pointer (:struct %:scene-components-locate-info-msft))
       (setf %:type :type-scene-components-locate-info-msft
             %:next ,next
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when time-p `(%:time ,time))
             ,@(when component-id-count-p `(%:component-id-count ,component-id-count))
             ,@(when component-ids-p `(%:component-ids ,component-ids)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-components-locate-info-msft %struct-types%)
      ':type-scene-components-locate-info-msft)

(defmacro with-scene-object-msft ((pointer &key %slots
                                             (object-type nil object-type-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-object-msft))
     (cffi:with-foreign-slots ((%:object-type)
                               ,pointer (:struct %:scene-object-msft))
       (setf ,@(when object-type-p `(%:object-type ,object-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-objects-msft ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (scene-object-count nil
                                               scene-object-count-p)
                                              (scene-objects nil
                                               scene-objects-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-objects-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:scene-object-count
                                %:scene-objects)
                               ,pointer (:struct %:scene-objects-msft))
       (setf %:type :type-scene-objects-msft
             %:next ,next
             ,@(when scene-object-count-p `(%:scene-object-count ,scene-object-count))
             ,@(when scene-objects-p `(%:scene-objects ,scene-objects)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-objects-msft %struct-types%)
      ':type-scene-objects-msft)

(defmacro with-scene-component-parent-filter-info-msft ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (parent-id
                                                                    nil
                                                                    parent-id-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-component-parent-filter-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:parent-id)
                               ,pointer (:struct %:scene-component-parent-filter-info-msft))
       (setf %:type :type-scene-component-parent-filter-info-msft
             %:next ,next
             ,@(when parent-id-p `(%:parent-id ,parent-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-component-parent-filter-info-msft %struct-types%)
      ':type-scene-component-parent-filter-info-msft)

(defmacro with-scene-object-types-filter-info-msft ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (object-type-count
                                                                nil
                                                                object-type-count-p)
                                                               (object-types
                                                                nil
                                                                object-types-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-object-types-filter-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:object-type-count
                                %:object-types)
                               ,pointer (:struct %:scene-object-types-filter-info-msft))
       (setf %:type :type-scene-object-types-filter-info-msft
             %:next ,next
             ,@(when object-type-count-p `(%:object-type-count ,object-type-count))
             ,@(when object-types-p `(%:object-types ,object-types)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-object-types-filter-info-msft %struct-types%)
      ':type-scene-object-types-filter-info-msft)

(defmacro with-scene-plane-msft ((pointer &key %slots
                                            (alignment nil alignment-p)
                                            (size nil size-p)
                                            (mesh-buffer-id nil
                                             mesh-buffer-id-p)
                                            (supports-indices-uint-16 nil
                                             supports-indices-uint-16-p))
                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-plane-msft))
     (cffi:with-foreign-slots ((%:alignment
                                %:size
                                %:mesh-buffer-id
                                %:supports-indices-uint-16)
                               ,pointer (:struct %:scene-plane-msft))
       (setf ,@(when alignment-p `(%:alignment ,alignment))
             ,@(when size-p `(%:size ,size))
             ,@(when mesh-buffer-id-p `(%:mesh-buffer-id ,mesh-buffer-id))
             ,@(when supports-indices-uint-16-p `(%:supports-indices-uint-16 ,supports-indices-uint-16)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-planes-msft ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (scene-plane-count nil
                                              scene-plane-count-p)
                                             (scene-planes nil scene-planes-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-planes-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:scene-plane-count
                                %:scene-planes)
                               ,pointer (:struct %:scene-planes-msft))
       (setf %:type :type-scene-planes-msft
             %:next ,next
             ,@(when scene-plane-count-p `(%:scene-plane-count ,scene-plane-count))
             ,@(when scene-planes-p `(%:scene-planes ,scene-planes)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-planes-msft %struct-types%)
      ':type-scene-planes-msft)

(defmacro with-scene-plane-alignment-filter-info-msft ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (alignment-count
                                                                   nil
                                                                   alignment-count-p)
                                                                  (alignments
                                                                   nil
                                                                   alignments-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-plane-alignment-filter-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:alignment-count %:alignments)
                               ,pointer (:struct %:scene-plane-alignment-filter-info-msft))
       (setf %:type :type-scene-plane-alignment-filter-info-msft
             %:next ,next
             ,@(when alignment-count-p `(%:alignment-count ,alignment-count))
             ,@(when alignments-p `(%:alignments ,alignments)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-plane-alignment-filter-info-msft %struct-types%)
      ':type-scene-plane-alignment-filter-info-msft)

(defmacro with-scene-mesh-msft ((pointer &key %slots
                                           (mesh-buffer-id nil
                                            mesh-buffer-id-p)
                                           (supports-indices-uint-16 nil
                                            supports-indices-uint-16-p))
                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-mesh-msft))
     (cffi:with-foreign-slots ((%:mesh-buffer-id %:supports-indices-uint-16)
                               ,pointer (:struct %:scene-mesh-msft))
       (setf ,@(when mesh-buffer-id-p `(%:mesh-buffer-id ,mesh-buffer-id))
             ,@(when supports-indices-uint-16-p `(%:supports-indices-uint-16 ,supports-indices-uint-16)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-meshes-msft ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (scene-mesh-count nil
                                              scene-mesh-count-p)
                                             (scene-meshes nil scene-meshes-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-meshes-msft))
     (cffi:with-foreign-slots ((%:type %:next %:scene-mesh-count %:scene-meshes)
                               ,pointer (:struct %:scene-meshes-msft))
       (setf %:type :type-scene-meshes-msft
             %:next ,next
             ,@(when scene-mesh-count-p `(%:scene-mesh-count ,scene-mesh-count))
             ,@(when scene-meshes-p `(%:scene-meshes ,scene-meshes)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-meshes-msft %struct-types%)
      ':type-scene-meshes-msft)

(defmacro with-scene-mesh-buffers-get-info-msft ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (mesh-buffer-id nil
                                                             mesh-buffer-id-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-mesh-buffers-get-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:mesh-buffer-id)
                               ,pointer (:struct %:scene-mesh-buffers-get-info-msft))
       (setf %:type :type-scene-mesh-buffers-get-info-msft
             %:next ,next
             ,@(when mesh-buffer-id-p `(%:mesh-buffer-id ,mesh-buffer-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-mesh-buffers-get-info-msft %struct-types%)
      ':type-scene-mesh-buffers-get-info-msft)

(defmacro with-scene-mesh-buffers-msft ((pointer &key %slots
                                                   (next '(cffi:null-pointer)))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-mesh-buffers-msft))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:scene-mesh-buffers-msft))
       (setf %:type :type-scene-mesh-buffers-msft
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-mesh-buffers-msft %struct-types%)
      ':type-scene-mesh-buffers-msft)

(defmacro with-scene-mesh-vertex-buffer-msft ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (vertex-capacity-input
                                                          nil
                                                          vertex-capacity-input-p)
                                                         (vertex-count-output
                                                          nil
                                                          vertex-count-output-p)
                                                         (vertices nil
                                                          vertices-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-mesh-vertex-buffer-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:vertex-capacity-input
                                %:vertex-count-output
                                %:vertices)
                               ,pointer (:struct %:scene-mesh-vertex-buffer-msft))
       (setf %:type :type-scene-mesh-vertex-buffer-msft
             %:next ,next
             ,@(when vertex-capacity-input-p `(%:vertex-capacity-input ,vertex-capacity-input))
             ,@(when vertex-count-output-p `(%:vertex-count-output ,vertex-count-output))
             ,@(when vertices-p `(%:vertices ,vertices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-mesh-vertex-buffer-msft %struct-types%)
      ':type-scene-mesh-vertex-buffer-msft)

(defmacro with-scene-mesh-indices-uint-32-msft ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (index-capacity-input
                                                            nil
                                                            index-capacity-input-p)
                                                           (index-count-output
                                                            nil
                                                            index-count-output-p)
                                                           (indices nil
                                                            indices-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-mesh-indices-uint-32-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:index-capacity-input
                                %:index-count-output
                                %:indices)
                               ,pointer (:struct %:scene-mesh-indices-uint-32-msft))
       (setf %:type :type-scene-mesh-indices-uint32-msft
             %:next ,next
             ,@(when index-capacity-input-p `(%:index-capacity-input ,index-capacity-input))
             ,@(when index-count-output-p `(%:index-count-output ,index-count-output))
             ,@(when indices-p `(%:indices ,indices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-mesh-indices-uint-32-msft %struct-types%)
      ':type-scene-mesh-indices-uint32-msft)

(defmacro with-scene-mesh-indices-uint-16-msft ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (index-capacity-input
                                                            nil
                                                            index-capacity-input-p)
                                                           (index-count-output
                                                            nil
                                                            index-count-output-p)
                                                           (indices nil
                                                            indices-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-mesh-indices-uint-16-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:index-capacity-input
                                %:index-count-output
                                %:indices)
                               ,pointer (:struct %:scene-mesh-indices-uint-16-msft))
       (setf %:type :type-scene-mesh-indices-uint16-msft
             %:next ,next
             ,@(when index-capacity-input-p `(%:index-capacity-input ,index-capacity-input))
             ,@(when index-count-output-p `(%:index-count-output ,index-count-output))
             ,@(when indices-p `(%:indices ,indices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-mesh-indices-uint-16-msft %struct-types%)
      ':type-scene-mesh-indices-uint16-msft)

(defmacro with-serialized-scene-fragment-data-get-info-msft ((pointer &key
                                                                        %slots
                                                                        (next
                                                                         '(cffi:null-pointer))
                                                                        (scene-fragment-id
                                                                         nil
                                                                         scene-fragment-id-p))
                                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:serialized-scene-fragment-data-get-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:scene-fragment-id)
                               ,pointer (:struct %:serialized-scene-fragment-data-get-info-msft))
       (setf %:type :type-serialized-scene-fragment-data-get-info-msft
             %:next ,next
             ,@(when scene-fragment-id-p `(%:scene-fragment-id ,scene-fragment-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:serialized-scene-fragment-data-get-info-msft %struct-types%)
      ':type-serialized-scene-fragment-data-get-info-msft)

(defmacro with-deserialize-scene-fragment-msft ((pointer &key %slots
                                                           (buffer-size nil
                                                            buffer-size-p)
                                                           (buffer nil
                                                            buffer-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:deserialize-scene-fragment-msft))
     (cffi:with-foreign-slots ((%:buffer-size %:buffer)
                               ,pointer (:struct %:deserialize-scene-fragment-msft))
       (setf ,@(when buffer-size-p `(%:buffer-size ,buffer-size))
             ,@(when buffer-p `(%:buffer ,buffer)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-scene-deserialize-info-msft ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (fragment-count nil
                                                        fragment-count-p)
                                                       (fragments nil
                                                        fragments-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-deserialize-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:fragment-count %:fragments)
                               ,pointer (:struct %:scene-deserialize-info-msft))
       (setf %:type :type-scene-deserialize-info-msft
             %:next ,next
             ,@(when fragment-count-p `(%:fragment-count ,fragment-count))
             ,@(when fragments-p `(%:fragments ,fragments)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-deserialize-info-msft %struct-types%)
      ':type-scene-deserialize-info-msft)

(defmacro with-system-color-space-properties-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (color-space nil
                                                             color-space-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-color-space-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:color-space)
                               ,pointer (:struct %:system-color-space-properties-fb))
       (setf %:type :type-system-color-space-properties-fb
             %:next ,next
             ,@(when color-space-p `(%:color-space ,color-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-color-space-properties-fb %struct-types%)
      ':type-system-color-space-properties-fb)

(defmacro with-system-spatial-entity-properties-fb ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (supports-spatial-entity
                                                                nil
                                                                supports-spatial-entity-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-spatial-entity-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-spatial-entity)
                               ,pointer (:struct %:system-spatial-entity-properties-fb))
       (setf %:type :type-system-spatial-entity-properties-fb
             %:next ,next
             ,@(when supports-spatial-entity-p `(%:supports-spatial-entity ,supports-spatial-entity)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-spatial-entity-properties-fb %struct-types%)
      ':type-system-spatial-entity-properties-fb)

(defmacro with-spatial-anchor-create-info-fb ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (space nil space-p)
                                                         (pose-in-space nil
                                                          pose-in-space-p)
                                                         (time nil time-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-anchor-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:space %:pose-in-space %:time)
                               ,pointer (:struct %:spatial-anchor-create-info-fb))
       (setf %:type :type-spatial-anchor-create-info-fb
             %:next ,next
             ,@(when space-p `(%:space ,space))
             ,@(when pose-in-space-p `(%:pose-in-space ,pose-in-space))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-anchor-create-info-fb %struct-types%)
      ':type-spatial-anchor-create-info-fb)

(defmacro with-space-component-status-set-info-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (component-type
                                                               nil
                                                               component-type-p)
                                                              (enabled nil
                                                               enabled-p)
                                                              (timeout nil
                                                               timeout-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-component-status-set-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:component-type
                                %:enabled
                                %:timeout)
                               ,pointer (:struct %:space-component-status-set-info-fb))
       (setf %:type :type-space-component-status-set-info-fb
             %:next ,next
             ,@(when component-type-p `(%:component-type ,component-type))
             ,@(when enabled-p `(%:enabled ,enabled))
             ,@(when timeout-p `(%:timeout ,timeout)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-component-status-set-info-fb %struct-types%)
      ':type-space-component-status-set-info-fb)

(defmacro with-space-component-status-fb ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (enabled nil enabled-p)
                                                     (change-pending nil
                                                      change-pending-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-component-status-fb))
     (cffi:with-foreign-slots ((%:type %:next %:enabled %:change-pending)
                               ,pointer (:struct %:space-component-status-fb))
       (setf %:type :type-space-component-status-fb
             %:next ,next
             ,@(when enabled-p `(%:enabled ,enabled))
             ,@(when change-pending-p `(%:change-pending ,change-pending)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-component-status-fb %struct-types%)
      ':type-space-component-status-fb)

(defmacro with-event-data-spatial-anchor-create-complete-fb ((pointer &key
                                                                        %slots
                                                                        (next
                                                                         '(cffi:null-pointer))
                                                                        (request-id
                                                                         nil
                                                                         request-id-p)
                                                                        (result
                                                                         nil
                                                                         result-p)
                                                                        (space
                                                                         nil
                                                                         space-p)
                                                                        (uuid
                                                                         nil
                                                                         uuid-p))
                                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-spatial-anchor-create-complete-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:request-id
                                %:result
                                %:space
                                %:uuid)
                               ,pointer (:struct %:event-data-spatial-anchor-create-complete-fb))
       (setf %:type :type-event-data-spatial-anchor-create-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result))
             ,@(when space-p `(%:space ,space))
             ,@(when uuid-p `(%:uuid ,uuid)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-spatial-anchor-create-complete-fb %struct-types%)
      ':type-event-data-spatial-anchor-create-complete-fb)

(defmacro with-event-data-space-set-status-complete-fb ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (request-id
                                                                    nil
                                                                    request-id-p)
                                                                   (result nil
                                                                    result-p)
                                                                   (space nil
                                                                    space-p)
                                                                   (uuid nil
                                                                    uuid-p)
                                                                   (component-type
                                                                    nil
                                                                    component-type-p)
                                                                   (enabled nil
                                                                    enabled-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-set-status-complete-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:request-id
                                %:result
                                %:space
                                %:uuid
                                %:component-type
                                %:enabled)
                               ,pointer (:struct %:event-data-space-set-status-complete-fb))
       (setf %:type :type-event-data-space-set-status-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result))
             ,@(when space-p `(%:space ,space))
             ,@(when uuid-p `(%:uuid ,uuid))
             ,@(when component-type-p `(%:component-type ,component-type))
             ,@(when enabled-p `(%:enabled ,enabled)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-set-status-complete-fb %struct-types%)
      ':type-event-data-space-set-status-complete-fb)

(defmacro with-foveation-profile-create-info-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer)))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-profile-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next)
                               ,pointer (:struct %:foveation-profile-create-info-fb))
       (setf %:type :type-foveation-profile-create-info-fb
             %:next ,next)
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-profile-create-info-fb %struct-types%)
      ':type-foveation-profile-create-info-fb)

(defmacro with-swapchain-create-info-foveation-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (flags nil
                                                               flags-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-create-info-foveation-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:swapchain-create-info-foveation-fb))
       (setf %:type :type-swapchain-create-info-foveation-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-create-info-foveation-fb %struct-types%)
      ':type-swapchain-create-info-foveation-fb)

(defmacro with-swapchain-state-foveation-fb ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer))
                                                        (flags nil flags-p)
                                                        (profile nil profile-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-state-foveation-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags %:profile)
                               ,pointer (:struct %:swapchain-state-foveation-fb))
       (setf %:type :type-swapchain-state-foveation-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags))
             ,@(when profile-p `(%:profile ,profile)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-state-foveation-fb %struct-types%)
      ':type-swapchain-state-foveation-fb)

(defmacro with-swapchain-image-foveation-vulkan-fb ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (image nil
                                                                image-p)
                                                               (width nil
                                                                width-p)
                                                               (height nil
                                                                height-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:swapchain-image-foveation-vulkan-fb))
     (cffi:with-foreign-slots ((%:type %:next %:image %:width %:height)
                               ,pointer (:struct %:swapchain-image-foveation-vulkan-fb))
       (setf %:type :type-swapchain-image-foveation-vulkan-fb
             %:next ,next
             ,@(when image-p `(%:image ,image))
             ,@(when width-p `(%:width ,width))
             ,@(when height-p `(%:height ,height)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:swapchain-image-foveation-vulkan-fb %struct-types%)
      ':type-swapchain-image-foveation-vulkan-fb)

(defmacro with-foveation-level-profile-create-info-fb ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (level nil
                                                                   level-p)
                                                                  (vertical-offset
                                                                   nil
                                                                   vertical-offset-p)
                                                                  (dynamic nil
                                                                   dynamic-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-level-profile-create-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:level
                                %:vertical-offset
                                %:dynamic)
                               ,pointer (:struct %:foveation-level-profile-create-info-fb))
       (setf %:type :type-foveation-level-profile-create-info-fb
             %:next ,next
             ,@(when level-p `(%:level ,level))
             ,@(when vertical-offset-p `(%:vertical-offset ,vertical-offset))
             ,@(when dynamic-p `(%:dynamic ,dynamic)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-level-profile-create-info-fb %struct-types%)
      ':type-foveation-level-profile-create-info-fb)

(defmacro with-foveation-eye-tracked-profile-create-info-meta ((pointer &key
                                                                          %slots
                                                                          (next
                                                                           '(cffi:null-pointer))
                                                                          (flags
                                                                           nil
                                                                           flags-p))
                                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-eye-tracked-profile-create-info-meta))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:foveation-eye-tracked-profile-create-info-meta))
       (setf %:type :type-foveation-eye-tracked-profile-create-info-meta
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-eye-tracked-profile-create-info-meta %struct-types%)
      ':type-foveation-eye-tracked-profile-create-info-meta)

(defmacro with-foveation-eye-tracked-state-meta ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (foveation-center
                                                             nil
                                                             foveation-center-p)
                                                            (flags nil flags-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-eye-tracked-state-meta))
     (cffi:with-foreign-slots ((%:type %:next %:foveation-center %:flags)
                               ,pointer (:struct %:foveation-eye-tracked-state-meta))
       (setf %:type :type-foveation-eye-tracked-state-meta
             %:next ,next
             ,@(when foveation-center-p `(%:foveation-center ,foveation-center))
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-eye-tracked-state-meta %struct-types%)
      ':type-foveation-eye-tracked-state-meta)

(defmacro with-system-foveation-eye-tracked-properties-meta ((pointer &key
                                                                        %slots
                                                                        (next
                                                                         '(cffi:null-pointer))
                                                                        (supports-foveation-eye-tracked
                                                                         nil
                                                                         supports-foveation-eye-tracked-p))
                                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-foveation-eye-tracked-properties-meta))
     (cffi:with-foreign-slots ((%:type %:next %:supports-foveation-eye-tracked)
                               ,pointer (:struct %:system-foveation-eye-tracked-properties-meta))
       (setf %:type :type-system-foveation-eye-tracked-properties-meta
             %:next ,next
             ,@(when supports-foveation-eye-tracked-p `(%:supports-foveation-eye-tracked ,supports-foveation-eye-tracked)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-foveation-eye-tracked-properties-meta %struct-types%)
      ':type-system-foveation-eye-tracked-properties-meta)

(defmacro with-vector-4s-fb ((pointer &key %slots (x nil x-p) (y nil y-p)
                                        (z nil z-p) (w nil w-p))
                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vector-4s-fb))
     (cffi:with-foreign-slots ((%:x %:y %:z %:w)
                               ,pointer (:struct %:vector-4s-fb))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y))
             ,@(when z-p `(%:z ,z))
             ,@(when w-p `(%:w ,w)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-hand-tracking-mesh-fb ((pointer &key %slots
                                                 (next '(cffi:null-pointer))
                                                 (joint-capacity-input nil
                                                  joint-capacity-input-p)
                                                 (joint-count-output nil
                                                  joint-count-output-p)
                                                 (joint-bind-poses nil
                                                  joint-bind-poses-p)
                                                 (joint-radi-i nil
                                                  joint-radi-i-p)
                                                 (joint-parents nil
                                                  joint-parents-p)
                                                 (vertex-capacity-input nil
                                                  vertex-capacity-input-p)
                                                 (vertex-count-output nil
                                                  vertex-count-output-p)
                                                 (vertex-positions nil
                                                  vertex-positions-p)
                                                 (vertex-normals nil
                                                  vertex-normals-p)
                                                 (vertex-uvs nil vertex-uvs-p)
                                                 (vertex-blend-indices nil
                                                  vertex-blend-indices-p)
                                                 (vertex-blend-weights nil
                                                  vertex-blend-weights-p)
                                                 (index-capacity-input nil
                                                  index-capacity-input-p)
                                                 (index-count-output nil
                                                  index-count-output-p)
                                                 (indices nil indices-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-tracking-mesh-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:joint-capacity-input
                                %:joint-count-output
                                %:joint-bind-poses
                                %:joint-radi-i
                                %:joint-parents
                                %:vertex-capacity-input
                                %:vertex-count-output
                                %:vertex-positions
                                %:vertex-normals
                                %:vertex-uvs
                                %:vertex-blend-indices
                                %:vertex-blend-weights
                                %:index-capacity-input
                                %:index-count-output
                                %:indices)
                               ,pointer (:struct %:hand-tracking-mesh-fb))
       (setf %:type :type-hand-tracking-mesh-fb
             %:next ,next
             ,@(when joint-capacity-input-p `(%:joint-capacity-input ,joint-capacity-input))
             ,@(when joint-count-output-p `(%:joint-count-output ,joint-count-output))
             ,@(when joint-bind-poses-p `(%:joint-bind-poses ,joint-bind-poses))
             ,@(when joint-radi-i-p `(%:joint-radi-i ,joint-radi-i))
             ,@(when joint-parents-p `(%:joint-parents ,joint-parents))
             ,@(when vertex-capacity-input-p `(%:vertex-capacity-input ,vertex-capacity-input))
             ,@(when vertex-count-output-p `(%:vertex-count-output ,vertex-count-output))
             ,@(when vertex-positions-p `(%:vertex-positions ,vertex-positions))
             ,@(when vertex-normals-p `(%:vertex-normals ,vertex-normals))
             ,@(when vertex-uvs-p `(%:vertex-uvs ,vertex-uvs))
             ,@(when vertex-blend-indices-p `(%:vertex-blend-indices ,vertex-blend-indices))
             ,@(when vertex-blend-weights-p `(%:vertex-blend-weights ,vertex-blend-weights))
             ,@(when index-capacity-input-p `(%:index-capacity-input ,index-capacity-input))
             ,@(when index-count-output-p `(%:index-count-output ,index-count-output))
             ,@(when indices-p `(%:indices ,indices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-tracking-mesh-fb %struct-types%)
      ':type-hand-tracking-mesh-fb)

(defmacro with-hand-tracking-scale-fb ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (sensor-output nil
                                                   sensor-output-p)
                                                  (current-output nil
                                                   current-output-p)
                                                  (override-hand-scale nil
                                                   override-hand-scale-p)
                                                  (override-value-input nil
                                                   override-value-input-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-tracking-scale-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:sensor-output
                                %:current-output
                                %:override-hand-scale
                                %:override-value-input)
                               ,pointer (:struct %:hand-tracking-scale-fb))
       (setf %:type :type-hand-tracking-scale-fb
             %:next ,next
             ,@(when sensor-output-p `(%:sensor-output ,sensor-output))
             ,@(when current-output-p `(%:current-output ,current-output))
             ,@(when override-hand-scale-p `(%:override-hand-scale ,override-hand-scale))
             ,@(when override-value-input-p `(%:override-value-input ,override-value-input)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-tracking-scale-fb %struct-types%)
      ':type-hand-tracking-scale-fb)

(defmacro with-hand-tracking-aim-state-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (status nil status-p)
                                                      (aim-pose nil aim-pose-p)
                                                      (pinch-strength-index nil
                                                       pinch-strength-index-p)
                                                      (pinch-strength-middle
                                                       nil
                                                       pinch-strength-middle-p)
                                                      (pinch-strength-ring nil
                                                       pinch-strength-ring-p)
                                                      (pinch-strength-little
                                                       nil
                                                       pinch-strength-little-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-tracking-aim-state-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:status
                                %:aim-pose
                                %:pinch-strength-index
                                %:pinch-strength-middle
                                %:pinch-strength-ring
                                %:pinch-strength-little)
                               ,pointer (:struct %:hand-tracking-aim-state-fb))
       (setf %:type :type-hand-tracking-aim-state-fb
             %:next ,next
             ,@(when status-p `(%:status ,status))
             ,@(when aim-pose-p `(%:aim-pose ,aim-pose))
             ,@(when pinch-strength-index-p `(%:pinch-strength-index ,pinch-strength-index))
             ,@(when pinch-strength-middle-p `(%:pinch-strength-middle ,pinch-strength-middle))
             ,@(when pinch-strength-ring-p `(%:pinch-strength-ring ,pinch-strength-ring))
             ,@(when pinch-strength-little-p `(%:pinch-strength-little ,pinch-strength-little)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-tracking-aim-state-fb %struct-types%)
      ':type-hand-tracking-aim-state-fb)

(defmacro with-hand-capsule-fb ((pointer &key %slots (points nil points-p)
                                           (radius nil radius-p)
                                           (joint nil joint-p))
                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-capsule-fb))
     (cffi:with-foreign-slots ((%:points %:radius %:joint)
                               ,pointer (:struct %:hand-capsule-fb))
       (setf ,@(when points-p `(%:points ,points))
             ,@(when radius-p `(%:radius ,radius))
             ,@(when joint-p `(%:joint ,joint)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-hand-tracking-capsules-state-fb ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (capsules nil
                                                            capsules-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:hand-tracking-capsules-state-fb))
     (cffi:with-foreign-slots ((%:type %:next %:capsules)
                               ,pointer (:struct %:hand-tracking-capsules-state-fb))
       (setf %:type :type-hand-tracking-capsules-state-fb
             %:next ,next
             ,@(when capsules-p `(%:capsules ,capsules)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:hand-tracking-capsules-state-fb %struct-types%)
      ':type-hand-tracking-capsules-state-fb)

(defmacro with-render-model-path-info-fb ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (path nil path-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:render-model-path-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:path)
                               ,pointer (:struct %:render-model-path-info-fb))
       (setf %:type :type-render-model-path-info-fb
             %:next ,next
             ,@(when path-p `(%:path ,path)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:render-model-path-info-fb %struct-types%)
      ':type-render-model-path-info-fb)

(defmacro with-render-model-properties-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (vendor-id nil
                                                       vendor-id-p)
                                                      (model-name nil
                                                       model-name-p)
                                                      (model-key nil
                                                       model-key-p)
                                                      (model-version nil
                                                       model-version-p)
                                                      (flags nil flags-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:render-model-properties-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:vendor-id
                                %:model-name
                                %:model-key
                                %:model-version
                                %:flags)
                               ,pointer (:struct %:render-model-properties-fb))
       (setf %:type :type-render-model-properties-fb
             %:next ,next
             ,@(when vendor-id-p `(%:vendor-id ,vendor-id))
             ,@(when model-key-p `(%:model-key ,model-key))
             ,@(when model-version-p `(%:model-version ,model-version))
             ,@(when flags-p `(%:flags ,flags)))
       (if (and ,model-name-p (not ,model-name))
           (setf %:model-name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,model-name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:render-model-properties-fb) '%:model-name)
            %:+max-render-model-name-size-fb+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:render-model-properties-fb %struct-types%)
      ':type-render-model-properties-fb)

(defmacro with-render-model-buffer-fb ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (buffer-capacity-input nil
                                                   buffer-capacity-input-p)
                                                  (buffer-count-output nil
                                                   buffer-count-output-p)
                                                  (buffer nil buffer-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:render-model-buffer-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:buffer-capacity-input
                                %:buffer-count-output
                                %:buffer)
                               ,pointer (:struct %:render-model-buffer-fb))
       (setf %:type :type-render-model-buffer-fb
             %:next ,next
             ,@(when buffer-capacity-input-p `(%:buffer-capacity-input ,buffer-capacity-input))
             ,@(when buffer-count-output-p `(%:buffer-count-output ,buffer-count-output))
             ,@(when buffer-p `(%:buffer ,buffer)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:render-model-buffer-fb %struct-types%)
      ':type-render-model-buffer-fb)

(defmacro with-render-model-load-info-fb ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (model-key nil
                                                      model-key-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:render-model-load-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:model-key)
                               ,pointer (:struct %:render-model-load-info-fb))
       (setf %:type :type-render-model-load-info-fb
             %:next ,next
             ,@(when model-key-p `(%:model-key ,model-key)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:render-model-load-info-fb %struct-types%)
      ':type-render-model-load-info-fb)

(defmacro with-system-render-model-properties-fb ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (supports-render-model-loading
                                                              nil
                                                              supports-render-model-loading-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-render-model-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-render-model-loading)
                               ,pointer (:struct %:system-render-model-properties-fb))
       (setf %:type :type-system-render-model-properties-fb
             %:next ,next
             ,@(when supports-render-model-loading-p `(%:supports-render-model-loading ,supports-render-model-loading)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-render-model-properties-fb %struct-types%)
      ':type-system-render-model-properties-fb)

(defmacro with-render-model-capabilities-request-fb ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (flags nil
                                                                 flags-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:render-model-capabilities-request-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:render-model-capabilities-request-fb))
       (setf %:type :type-render-model-capabilities-request-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:render-model-capabilities-request-fb %struct-types%)
      ':type-render-model-capabilities-request-fb)

(defmacro with-space-query-info-fb ((pointer &key %slots
                                               (next '(cffi:null-pointer))
                                               (query-action nil
                                                query-action-p)
                                               (max-result-count nil
                                                max-result-count-p)
                                               (timeout nil timeout-p)
                                               (filter nil filter-p)
                                               (exclude-filter nil
                                                exclude-filter-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-query-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:query-action
                                %:max-result-count
                                %:timeout
                                %:filter
                                %:exclude-filter)
                               ,pointer (:struct %:space-query-info-fb))
       (setf %:type :type-space-query-info-fb
             %:next ,next
             ,@(when query-action-p `(%:query-action ,query-action))
             ,@(when max-result-count-p `(%:max-result-count ,max-result-count))
             ,@(when timeout-p `(%:timeout ,timeout))
             ,@(when filter-p `(%:filter ,filter))
             ,@(when exclude-filter-p `(%:exclude-filter ,exclude-filter)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-query-info-fb %struct-types%)
      ':type-space-query-info-fb)

(defmacro with-space-storage-location-filter-info-fb ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (location nil
                                                                  location-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-storage-location-filter-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:location)
                               ,pointer (:struct %:space-storage-location-filter-info-fb))
       (setf %:type :type-space-storage-location-filter-info-fb
             %:next ,next
             ,@(when location-p `(%:location ,location)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-storage-location-filter-info-fb %struct-types%)
      ':type-space-storage-location-filter-info-fb)

(defmacro with-space-uuid-filter-info-fb ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (uuid-count nil
                                                      uuid-count-p)
                                                     (uuids nil uuids-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-uuid-filter-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:uuid-count %:uuids)
                               ,pointer (:struct %:space-uuid-filter-info-fb))
       (setf %:type :type-space-uuid-filter-info-fb
             %:next ,next
             ,@(when uuid-count-p `(%:uuid-count ,uuid-count))
             ,@(when uuids-p `(%:uuids ,uuids)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-uuid-filter-info-fb %struct-types%)
      ':type-space-uuid-filter-info-fb)

(defmacro with-space-component-filter-info-fb ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (component-type nil
                                                           component-type-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-component-filter-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:component-type)
                               ,pointer (:struct %:space-component-filter-info-fb))
       (setf %:type :type-space-component-filter-info-fb
             %:next ,next
             ,@(when component-type-p `(%:component-type ,component-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-component-filter-info-fb %struct-types%)
      ':type-space-component-filter-info-fb)

(defmacro with-space-query-result-fb ((pointer &key %slots (space nil space-p)
                                                 (uuid nil uuid-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-query-result-fb))
     (cffi:with-foreign-slots ((%:space %:uuid)
                               ,pointer (:struct %:space-query-result-fb))
       (setf ,@(when space-p `(%:space ,space))
             ,@(when uuid-p `(%:uuid ,uuid)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-space-query-results-fb ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (result-capacity-input nil
                                                   result-capacity-input-p)
                                                  (result-count-output nil
                                                   result-count-output-p)
                                                  (results nil results-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-query-results-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:result-capacity-input
                                %:result-count-output
                                %:results)
                               ,pointer (:struct %:space-query-results-fb))
       (setf %:type :type-space-query-results-fb
             %:next ,next
             ,@(when result-capacity-input-p `(%:result-capacity-input ,result-capacity-input))
             ,@(when result-count-output-p `(%:result-count-output ,result-count-output))
             ,@(when results-p `(%:results ,results)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-query-results-fb %struct-types%)
      ':type-space-query-results-fb)

(defmacro with-event-data-space-query-results-available-fb ((pointer &key
                                                                       %slots
                                                                       (next
                                                                        '(cffi:null-pointer))
                                                                       (request-id
                                                                        nil
                                                                        request-id-p))
                                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-query-results-available-fb))
     (cffi:with-foreign-slots ((%:type %:next %:request-id)
                               ,pointer (:struct %:event-data-space-query-results-available-fb))
       (setf %:type :type-event-data-space-query-results-available-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-query-results-available-fb %struct-types%)
      ':type-event-data-space-query-results-available-fb)

(defmacro with-event-data-space-query-complete-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (request-id nil
                                                               request-id-p)
                                                              (result nil
                                                               result-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-query-complete-fb))
     (cffi:with-foreign-slots ((%:type %:next %:request-id %:result)
                               ,pointer (:struct %:event-data-space-query-complete-fb))
       (setf %:type :type-event-data-space-query-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-query-complete-fb %struct-types%)
      ':type-event-data-space-query-complete-fb)

(defmacro with-space-save-info-fb ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (space nil space-p)
                                              (location nil location-p)
                                              (persistence-mode nil
                                               persistence-mode-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-save-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:space
                                %:location
                                %:persistence-mode)
                               ,pointer (:struct %:space-save-info-fb))
       (setf %:type :type-space-save-info-fb
             %:next ,next
             ,@(when space-p `(%:space ,space))
             ,@(when location-p `(%:location ,location))
             ,@(when persistence-mode-p `(%:persistence-mode ,persistence-mode)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-save-info-fb %struct-types%)
      ':type-space-save-info-fb)

(defmacro with-space-erase-info-fb ((pointer &key %slots
                                               (next '(cffi:null-pointer))
                                               (space nil space-p)
                                               (location nil location-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-erase-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:space %:location)
                               ,pointer (:struct %:space-erase-info-fb))
       (setf %:type :type-space-erase-info-fb
             %:next ,next
             ,@(when space-p `(%:space ,space))
             ,@(when location-p `(%:location ,location)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-erase-info-fb %struct-types%)
      ':type-space-erase-info-fb)

(defmacro with-event-data-space-save-complete-fb ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (request-id nil
                                                              request-id-p)
                                                             (result nil
                                                              result-p)
                                                             (space nil
                                                              space-p)
                                                             (uuid nil uuid-p)
                                                             (location nil
                                                              location-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-save-complete-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:request-id
                                %:result
                                %:space
                                %:uuid
                                %:location)
                               ,pointer (:struct %:event-data-space-save-complete-fb))
       (setf %:type :type-event-data-space-save-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result))
             ,@(when space-p `(%:space ,space))
             ,@(when uuid-p `(%:uuid ,uuid))
             ,@(when location-p `(%:location ,location)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-save-complete-fb %struct-types%)
      ':type-event-data-space-save-complete-fb)

(defmacro with-event-data-space-erase-complete-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (request-id nil
                                                               request-id-p)
                                                              (result nil
                                                               result-p)
                                                              (space nil
                                                               space-p)
                                                              (uuid nil uuid-p)
                                                              (location nil
                                                               location-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-erase-complete-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:request-id
                                %:result
                                %:space
                                %:uuid
                                %:location)
                               ,pointer (:struct %:event-data-space-erase-complete-fb))
       (setf %:type :type-event-data-space-erase-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result))
             ,@(when space-p `(%:space ,space))
             ,@(when uuid-p `(%:uuid ,uuid))
             ,@(when location-p `(%:location ,location)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-erase-complete-fb %struct-types%)
      ':type-event-data-space-erase-complete-fb)

(defmacro with-space-share-info-fb ((pointer &key %slots
                                               (next '(cffi:null-pointer))
                                               (space-count nil space-count-p)
                                               (spaces nil spaces-p)
                                               (user-count nil user-count-p)
                                               (users nil users-p))
                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-share-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:space-count
                                %:spaces
                                %:user-count
                                %:users)
                               ,pointer (:struct %:space-share-info-fb))
       (setf %:type :type-space-share-info-fb
             %:next ,next
             ,@(when space-count-p `(%:space-count ,space-count))
             ,@(when spaces-p `(%:spaces ,spaces))
             ,@(when user-count-p `(%:user-count ,user-count))
             ,@(when users-p `(%:users ,users)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-share-info-fb %struct-types%)
      ':type-space-share-info-fb)

(defmacro with-event-data-space-share-complete-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (request-id nil
                                                               request-id-p)
                                                              (result nil
                                                               result-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-share-complete-fb))
     (cffi:with-foreign-slots ((%:type %:next %:request-id %:result)
                               ,pointer (:struct %:event-data-space-share-complete-fb))
       (setf %:type :type-event-data-space-share-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-share-complete-fb %struct-types%)
      ':type-event-data-space-share-complete-fb)

(defmacro with-space-list-save-info-fb ((pointer &key %slots
                                                   (next '(cffi:null-pointer))
                                                   (space-count nil
                                                    space-count-p)
                                                   (spaces nil spaces-p)
                                                   (location nil location-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-list-save-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:space-count %:spaces %:location)
                               ,pointer (:struct %:space-list-save-info-fb))
       (setf %:type :type-space-list-save-info-fb
             %:next ,next
             ,@(when space-count-p `(%:space-count ,space-count))
             ,@(when spaces-p `(%:spaces ,spaces))
             ,@(when location-p `(%:location ,location)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-list-save-info-fb %struct-types%)
      ':type-space-list-save-info-fb)

(defmacro with-event-data-space-list-save-complete-fb ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (request-id
                                                                   nil
                                                                   request-id-p)
                                                                  (result nil
                                                                   result-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-space-list-save-complete-fb))
     (cffi:with-foreign-slots ((%:type %:next %:request-id %:result)
                               ,pointer (:struct %:event-data-space-list-save-complete-fb))
       (setf %:type :type-event-data-space-list-save-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-space-list-save-complete-fb %struct-types%)
      ':type-event-data-space-list-save-complete-fb)

(defmacro with-space-container-fb ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (uuid-capacity-input nil
                                               uuid-capacity-input-p)
                                              (uuid-count-output nil
                                               uuid-count-output-p)
                                              (uuids nil uuids-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-container-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:uuid-capacity-input
                                %:uuid-count-output
                                %:uuids)
                               ,pointer (:struct %:space-container-fb))
       (setf %:type :type-space-container-fb
             %:next ,next
             ,@(when uuid-capacity-input-p `(%:uuid-capacity-input ,uuid-capacity-input))
             ,@(when uuid-count-output-p `(%:uuid-count-output ,uuid-count-output))
             ,@(when uuids-p `(%:uuids ,uuids)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-container-fb %struct-types%)
      ':type-space-container-fb)

(defmacro with-extent-3d-f-fb ((pointer &key %slots (width nil width-p)
                                          (height nil height-p)
                                          (depth nil depth-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:extent-3d-f-fb))
     (cffi:with-foreign-slots ((%:width %:height %:depth)
                               ,pointer (:struct %:extent-3d-f-fb))
       (setf ,@(when width-p `(%:width ,width))
             ,@(when height-p `(%:height ,height))
             ,@(when depth-p `(%:depth ,depth)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-offset-3d-f-fb ((pointer &key %slots (x nil x-p) (y nil y-p)
                                          (z nil z-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:offset-3d-f-fb))
     (cffi:with-foreign-slots ((%:x %:y %:z)
                               ,pointer (:struct %:offset-3d-f-fb))
       (setf ,@(when x-p `(%:x ,x))
             ,@(when y-p `(%:y ,y))
             ,@(when z-p `(%:z ,z)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-rect-3d-f-fb ((pointer &key %slots (offset nil offset-p)
                                        (extent nil extent-p))
                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:rect-3d-f-fb))
     (cffi:with-foreign-slots ((%:offset %:extent)
                               ,pointer (:struct %:rect-3d-f-fb))
       (setf ,@(when offset-p `(%:offset ,offset))
             ,@(when extent-p `(%:extent ,extent)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-semantic-labels-fb ((pointer &key %slots
                                              (next '(cffi:null-pointer))
                                              (buffer-capacity-input nil
                                               buffer-capacity-input-p)
                                              (buffer-count-output nil
                                               buffer-count-output-p)
                                              (buffer nil buffer-p))
                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:semantic-labels-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:buffer-capacity-input
                                %:buffer-count-output
                                %:buffer)
                               ,pointer (:struct %:semantic-labels-fb))
       (setf %:type :type-semantic-labels-fb
             %:next ,next
             ,@(when buffer-capacity-input-p `(%:buffer-capacity-input ,buffer-capacity-input))
             ,@(when buffer-count-output-p `(%:buffer-count-output ,buffer-count-output))
             ,@(when buffer-p `(%:buffer ,buffer)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:semantic-labels-fb %struct-types%)
      ':type-semantic-labels-fb)

(defmacro with-room-layout-fb ((pointer &key %slots (next '(cffi:null-pointer))
                                          (floor-uuid nil floor-uuid-p)
                                          (ceiling-uuid nil ceiling-uuid-p)
                                          (wall-uuid-capacity-input nil
                                           wall-uuid-capacity-input-p)
                                          (wall-uuid-count-output nil
                                           wall-uuid-count-output-p)
                                          (wall-uuids nil wall-uuids-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:room-layout-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:floor-uuid
                                %:ceiling-uuid
                                %:wall-uuid-capacity-input
                                %:wall-uuid-count-output
                                %:wall-uuids)
                               ,pointer (:struct %:room-layout-fb))
       (setf %:type :type-room-layout-fb
             %:next ,next
             ,@(when floor-uuid-p `(%:floor-uuid ,floor-uuid))
             ,@(when ceiling-uuid-p `(%:ceiling-uuid ,ceiling-uuid))
             ,@(when wall-uuid-capacity-input-p `(%:wall-uuid-capacity-input ,wall-uuid-capacity-input))
             ,@(when wall-uuid-count-output-p `(%:wall-uuid-count-output ,wall-uuid-count-output))
             ,@(when wall-uuids-p `(%:wall-uuids ,wall-uuids)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:room-layout-fb %struct-types%)
      ':type-room-layout-fb)

(defmacro with-boundary-2d-fb ((pointer &key %slots (next '(cffi:null-pointer))
                                          (vertex-capacity-input nil
                                           vertex-capacity-input-p)
                                          (vertex-count-output nil
                                           vertex-count-output-p)
                                          (vertices nil vertices-p))
                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:boundary-2d-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:vertex-capacity-input
                                %:vertex-count-output
                                %:vertices)
                               ,pointer (:struct %:boundary-2d-fb))
       (setf %:type :type-boundary-2d-fb
             %:next ,next
             ,@(when vertex-capacity-input-p `(%:vertex-capacity-input ,vertex-capacity-input))
             ,@(when vertex-count-output-p `(%:vertex-count-output ,vertex-count-output))
             ,@(when vertices-p `(%:vertices ,vertices)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:boundary-2d-fb %struct-types%)
      ':type-boundary-2d-fb)

(defmacro with-scene-capture-request-info-fb ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (request-byte-count
                                                          nil
                                                          request-byte-count-p)
                                                         (request nil
                                                          request-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:scene-capture-request-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:request-byte-count %:request)
                               ,pointer (:struct %:scene-capture-request-info-fb))
       (setf %:type :type-scene-capture-request-info-fb
             %:next ,next
             ,@(when request-byte-count-p `(%:request-byte-count ,request-byte-count))
             ,@(when request-p `(%:request ,request)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:scene-capture-request-info-fb %struct-types%)
      ':type-scene-capture-request-info-fb)

(defmacro with-event-data-scene-capture-complete-fb ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (request-id nil
                                                                 request-id-p)
                                                                (result nil
                                                                 result-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-scene-capture-complete-fb))
     (cffi:with-foreign-slots ((%:type %:next %:request-id %:result)
                               ,pointer (:struct %:event-data-scene-capture-complete-fb))
       (setf %:type :type-event-data-scene-capture-complete-fb
             %:next ,next
             ,@(when request-id-p `(%:request-id ,request-id))
             ,@(when result-p `(%:result ,result)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-scene-capture-complete-fb %struct-types%)
      ':type-event-data-scene-capture-complete-fb)

(defmacro with-system-keyboard-tracking-properties-fb ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (supports-keyboard-tracking
                                                                   nil
                                                                   supports-keyboard-tracking-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-keyboard-tracking-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-keyboard-tracking)
                               ,pointer (:struct %:system-keyboard-tracking-properties-fb))
       (setf %:type :type-system-keyboard-tracking-properties-fb
             %:next ,next
             ,@(when supports-keyboard-tracking-p `(%:supports-keyboard-tracking ,supports-keyboard-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-keyboard-tracking-properties-fb %struct-types%)
      ':type-system-keyboard-tracking-properties-fb)

(defmacro with-keyboard-tracking-description-fb ((pointer &key %slots
                                                            (tracked-keyboard-id
                                                             nil
                                                             tracked-keyboard-id-p)
                                                            (size nil size-p)
                                                            (flags nil flags-p)
                                                            (name nil name-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:keyboard-tracking-description-fb))
     (cffi:with-foreign-slots ((%:tracked-keyboard-id %:size %:flags %:name)
                               ,pointer (:struct %:keyboard-tracking-description-fb))
       (setf ,@(when tracked-keyboard-id-p `(%:tracked-keyboard-id ,tracked-keyboard-id))
             ,@(when size-p `(%:size ,size))
             ,@(when flags-p `(%:flags ,flags)))
       (if (and ,name-p (not ,name))
           (setf %:name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:keyboard-tracking-description-fb) '%:name)
            %:+max-keyboard-tracking-name-size-fb+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-keyboard-space-create-info-fb ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (tracked-keyboard-id
                                                          nil
                                                          tracked-keyboard-id-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:keyboard-space-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:tracked-keyboard-id)
                               ,pointer (:struct %:keyboard-space-create-info-fb))
       (setf %:type :type-keyboard-space-create-info-fb
             %:next ,next
             ,@(when tracked-keyboard-id-p `(%:tracked-keyboard-id ,tracked-keyboard-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:keyboard-space-create-info-fb %struct-types%)
      ':type-keyboard-space-create-info-fb)

(defmacro with-keyboard-tracking-query-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (flags nil flags-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:keyboard-tracking-query-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:keyboard-tracking-query-fb))
       (setf %:type :type-keyboard-tracking-query-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:keyboard-tracking-query-fb %struct-types%)
      ':type-keyboard-tracking-query-fb)

(defmacro with-composition-layer-depth-test-varjo ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (depth-test-range-near-z
                                                               nil
                                                               depth-test-range-near-z-p)
                                                              (depth-test-range-far-z
                                                               nil
                                                               depth-test-range-far-z-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-depth-test-varjo))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:depth-test-range-near-z
                                %:depth-test-range-far-z)
                               ,pointer (:struct %:composition-layer-depth-test-varjo))
       (setf %:type :type-composition-layer-depth-test-varjo
             %:next ,next
             ,@(when depth-test-range-near-z-p `(%:depth-test-range-near-z ,depth-test-range-near-z))
             ,@(when depth-test-range-far-z-p `(%:depth-test-range-far-z ,depth-test-range-far-z)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-depth-test-varjo %struct-types%)
      ':type-composition-layer-depth-test-varjo)

(defmacro with-view-locate-foveated-rendering-varjo ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (foveated-rendering-active
                                                                 nil
                                                                 foveated-rendering-active-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:view-locate-foveated-rendering-varjo))
     (cffi:with-foreign-slots ((%:type %:next %:foveated-rendering-active)
                               ,pointer (:struct %:view-locate-foveated-rendering-varjo))
       (setf %:type :type-view-locate-foveated-rendering-varjo
             %:next ,next
             ,@(when foveated-rendering-active-p `(%:foveated-rendering-active ,foveated-rendering-active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:view-locate-foveated-rendering-varjo %struct-types%)
      ':type-view-locate-foveated-rendering-varjo)

(defmacro with-foveated-view-configuration-view-varjo ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (foveated-rendering-active
                                                                   nil
                                                                   foveated-rendering-active-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveated-view-configuration-view-varjo))
     (cffi:with-foreign-slots ((%:type %:next %:foveated-rendering-active)
                               ,pointer (:struct %:foveated-view-configuration-view-varjo))
       (setf %:type :type-foveated-view-configuration-view-varjo
             %:next ,next
             ,@(when foveated-rendering-active-p `(%:foveated-rendering-active ,foveated-rendering-active)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveated-view-configuration-view-varjo %struct-types%)
      ':type-foveated-view-configuration-view-varjo)

(defmacro with-system-foveated-rendering-properties-varjo ((pointer &key %slots
                                                                      (next
                                                                       '(cffi:null-pointer))
                                                                      (supports-foveated-rendering
                                                                       nil
                                                                       supports-foveated-rendering-p))
                                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-foveated-rendering-properties-varjo))
     (cffi:with-foreign-slots ((%:type %:next %:supports-foveated-rendering)
                               ,pointer (:struct %:system-foveated-rendering-properties-varjo))
       (setf %:type :type-system-foveated-rendering-properties-varjo
             %:next ,next
             ,@(when supports-foveated-rendering-p `(%:supports-foveated-rendering ,supports-foveated-rendering)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-foveated-rendering-properties-varjo %struct-types%)
      ':type-system-foveated-rendering-properties-varjo)

(defmacro with-composition-layer-reprojection-info-msft ((pointer &key %slots
                                                                    (next
                                                                     '(cffi:null-pointer))
                                                                    (reprojection-mode
                                                                     nil
                                                                     reprojection-mode-p))
                                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-reprojection-info-msft))
     (cffi:with-foreign-slots ((%:type %:next %:reprojection-mode)
                               ,pointer (:struct %:composition-layer-reprojection-info-msft))
       (setf %:type :type-composition-layer-reprojection-info-msft
             %:next ,next
             ,@(when reprojection-mode-p `(%:reprojection-mode ,reprojection-mode)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-reprojection-info-msft %struct-types%)
      ':type-composition-layer-reprojection-info-msft)

(defmacro with-composition-layer-reprojection-plane-override-msft ((pointer &key
                                                                              %slots
                                                                              (next
                                                                               '(cffi:null-pointer))
                                                                              (position
                                                                               nil
                                                                               position-p)
                                                                              (normal
                                                                               nil
                                                                               normal-p)
                                                                              (velocity
                                                                               nil
                                                                               velocity-p))
                                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-reprojection-plane-override-msft))
     (cffi:with-foreign-slots ((%:type %:next %:position %:normal %:velocity)
                               ,pointer (:struct %:composition-layer-reprojection-plane-override-msft))
       (setf %:type :type-composition-layer-reprojection-plane-override-msft
             %:next ,next
             ,@(when position-p `(%:position ,position))
             ,@(when normal-p `(%:normal ,normal))
             ,@(when velocity-p `(%:velocity ,velocity)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-reprojection-plane-override-msft %struct-types%)
      ':type-composition-layer-reprojection-plane-override-msft)

(defmacro with-triangle-mesh-create-info-fb ((pointer &key %slots
                                                        (next
                                                         '(cffi:null-pointer))
                                                        (flags nil flags-p)
                                                        (winding-order nil
                                                         winding-order-p)
                                                        (vertex-count nil
                                                         vertex-count-p)
                                                        (vertex-buffer nil
                                                         vertex-buffer-p)
                                                        (triangle-count nil
                                                         triangle-count-p)
                                                        (index-buffer nil
                                                         index-buffer-p))
                                             &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:triangle-mesh-create-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:flags
                                %:winding-order
                                %:vertex-count
                                %:vertex-buffer
                                %:triangle-count
                                %:index-buffer)
                               ,pointer (:struct %:triangle-mesh-create-info-fb))
       (setf %:type :type-triangle-mesh-create-info-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags))
             ,@(when winding-order-p `(%:winding-order ,winding-order))
             ,@(when vertex-count-p `(%:vertex-count ,vertex-count))
             ,@(when vertex-buffer-p `(%:vertex-buffer ,vertex-buffer))
             ,@(when triangle-count-p `(%:triangle-count ,triangle-count))
             ,@(when index-buffer-p `(%:index-buffer ,index-buffer)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:triangle-mesh-create-info-fb %struct-types%)
      ':type-triangle-mesh-create-info-fb)

(defmacro with-system-passthrough-properties-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (supports-passthrough
                                                             nil
                                                             supports-passthrough-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-passthrough-properties-fb))
     (cffi:with-foreign-slots ((%:type %:next %:supports-passthrough)
                               ,pointer (:struct %:system-passthrough-properties-fb))
       (setf %:type :type-system-passthrough-properties-fb
             %:next ,next
             ,@(when supports-passthrough-p `(%:supports-passthrough ,supports-passthrough)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-passthrough-properties-fb %struct-types%)
      ':type-system-passthrough-properties-fb)

(defmacro with-system-passthrough-properties-2-fb ((pointer &key %slots
                                                              (next
                                                               '(cffi:null-pointer))
                                                              (capabilities nil
                                                               capabilities-p))
                                                   &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-passthrough-properties-2-fb))
     (cffi:with-foreign-slots ((%:type %:next %:capabilities)
                               ,pointer (:struct %:system-passthrough-properties-2-fb))
       (setf %:type :type-system-passthrough-properties2-fb
             %:next ,next
             ,@(when capabilities-p `(%:capabilities ,capabilities)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-passthrough-properties-2-fb %struct-types%)
      ':type-system-passthrough-properties2-fb)

(defmacro with-passthrough-create-info-fb ((pointer &key %slots
                                                      (next
                                                       '(cffi:null-pointer))
                                                      (flags nil flags-p))
                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:passthrough-create-info-fb))
       (setf %:type :type-passthrough-create-info-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-create-info-fb %struct-types%)
      ':type-passthrough-create-info-fb)

(defmacro with-passthrough-layer-create-info-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (passthrough nil
                                                             passthrough-p)
                                                            (flags nil flags-p)
                                                            (purpose nil
                                                             purpose-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-layer-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:passthrough %:flags %:purpose)
                               ,pointer (:struct %:passthrough-layer-create-info-fb))
       (setf %:type :type-passthrough-layer-create-info-fb
             %:next ,next
             ,@(when passthrough-p `(%:passthrough ,passthrough))
             ,@(when flags-p `(%:flags ,flags))
             ,@(when purpose-p `(%:purpose ,purpose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-layer-create-info-fb %struct-types%)
      ':type-passthrough-layer-create-info-fb)

(defmacro with-composition-layer-passthrough-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (flags nil flags-p)
                                                            (space nil space-p)
                                                            (layer-handle nil
                                                             layer-handle-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-passthrough-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags %:space %:layer-handle)
                               ,pointer (:struct %:composition-layer-passthrough-fb))
       (setf %:type :type-composition-layer-passthrough-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags))
             ,@(when space-p `(%:space ,space))
             ,@(when layer-handle-p `(%:layer-handle ,layer-handle)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-passthrough-fb %struct-types%)
      ':type-composition-layer-passthrough-fb)

(defmacro with-geometry-instance-create-info-fb ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (layer nil layer-p)
                                                            (mesh nil mesh-p)
                                                            (base-space nil
                                                             base-space-p)
                                                            (pose nil pose-p)
                                                            (scale nil scale-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:geometry-instance-create-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer
                                %:mesh
                                %:base-space
                                %:pose
                                %:scale)
                               ,pointer (:struct %:geometry-instance-create-info-fb))
       (setf %:type :type-geometry-instance-create-info-fb
             %:next ,next
             ,@(when layer-p `(%:layer ,layer))
             ,@(when mesh-p `(%:mesh ,mesh))
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when scale-p `(%:scale ,scale)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:geometry-instance-create-info-fb %struct-types%)
      ':type-geometry-instance-create-info-fb)

(defmacro with-geometry-instance-transform-fb ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (base-space nil
                                                           base-space-p)
                                                          (time nil time-p)
                                                          (pose nil pose-p)
                                                          (scale nil scale-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:geometry-instance-transform-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:base-space
                                %:time
                                %:pose
                                %:scale)
                               ,pointer (:struct %:geometry-instance-transform-fb))
       (setf %:type :type-geometry-instance-transform-fb
             %:next ,next
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when time-p `(%:time ,time))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when scale-p `(%:scale ,scale)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:geometry-instance-transform-fb %struct-types%)
      ':type-geometry-instance-transform-fb)

(defmacro with-passthrough-style-fb ((pointer &key %slots
                                                (next '(cffi:null-pointer))
                                                (texture-opacity-factor nil
                                                 texture-opacity-factor-p)
                                                (edge-color nil edge-color-p))
                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-style-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:texture-opacity-factor
                                %:edge-color)
                               ,pointer (:struct %:passthrough-style-fb))
       (setf %:type :type-passthrough-style-fb
             %:next ,next
             ,@(when texture-opacity-factor-p `(%:texture-opacity-factor ,texture-opacity-factor))
             ,@(when edge-color-p `(%:edge-color ,edge-color)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-style-fb %struct-types%)
      ':type-passthrough-style-fb)

(defmacro with-passthrough-color-map-mono-to-rgba-fb ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (texture-color-map
                                                                  nil
                                                                  texture-color-map-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-color-map-mono-to-rgba-fb))
     (cffi:with-foreign-slots ((%:type %:next %:texture-color-map)
                               ,pointer (:struct %:passthrough-color-map-mono-to-rgba-fb))
       (setf %:type :type-passthrough-color-map-mono-to-rgba-fb
             %:next ,next
             ,@(when texture-color-map-p `(%:texture-color-map ,texture-color-map)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-color-map-mono-to-rgba-fb %struct-types%)
      ':type-passthrough-color-map-mono-to-rgba-fb)

(defmacro with-passthrough-color-map-mono-to-mono-fb ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (texture-color-map
                                                                  nil
                                                                  texture-color-map-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-color-map-mono-to-mono-fb))
     (cffi:with-foreign-slots ((%:type %:next %:texture-color-map)
                               ,pointer (:struct %:passthrough-color-map-mono-to-mono-fb))
       (setf %:type :type-passthrough-color-map-mono-to-mono-fb
             %:next ,next
             ,@(when texture-color-map-p `(%:texture-color-map ,texture-color-map)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-color-map-mono-to-mono-fb %struct-types%)
      ':type-passthrough-color-map-mono-to-mono-fb)

(defmacro with-passthrough-brightness-contrast-saturation-fb ((pointer &key
                                                                         %slots
                                                                         (next
                                                                          '(cffi:null-pointer))
                                                                         (brightness
                                                                          nil
                                                                          brightness-p)
                                                                         (contrast
                                                                          nil
                                                                          contrast-p)
                                                                         (saturation
                                                                          nil
                                                                          saturation-p))
                                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-brightness-contrast-saturation-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:brightness
                                %:contrast
                                %:saturation)
                               ,pointer (:struct %:passthrough-brightness-contrast-saturation-fb))
       (setf %:type :type-passthrough-brightness-contrast-saturation-fb
             %:next ,next
             ,@(when brightness-p `(%:brightness ,brightness))
             ,@(when contrast-p `(%:contrast ,contrast))
             ,@(when saturation-p `(%:saturation ,saturation)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-brightness-contrast-saturation-fb %struct-types%)
      ':type-passthrough-brightness-contrast-saturation-fb)

(defmacro with-event-data-passthrough-state-changed-fb ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (flags nil
                                                                    flags-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-passthrough-state-changed-fb))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:event-data-passthrough-state-changed-fb))
       (setf %:type :type-event-data-passthrough-state-changed-fb
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-passthrough-state-changed-fb %struct-types%)
      ':type-event-data-passthrough-state-changed-fb)

(defmacro with-passthrough-keyboard-hands-intensity-fb ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (left-hand-intensity
                                                                    nil
                                                                    left-hand-intensity-p)
                                                                   (right-hand-intensity
                                                                    nil
                                                                    right-hand-intensity-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-keyboard-hands-intensity-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:left-hand-intensity
                                %:right-hand-intensity)
                               ,pointer (:struct %:passthrough-keyboard-hands-intensity-fb))
       (setf %:type :type-passthrough-keyboard-hands-intensity-fb
             %:next ,next
             ,@(when left-hand-intensity-p `(%:left-hand-intensity ,left-hand-intensity))
             ,@(when right-hand-intensity-p `(%:right-hand-intensity ,right-hand-intensity)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-keyboard-hands-intensity-fb %struct-types%)
      ':type-passthrough-keyboard-hands-intensity-fb)

(defmacro with-local-dimming-frame-end-info-meta ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (local-dimming-mode
                                                              nil
                                                              local-dimming-mode-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:local-dimming-frame-end-info-meta))
     (cffi:with-foreign-slots ((%:type %:next %:local-dimming-mode)
                               ,pointer (:struct %:local-dimming-frame-end-info-meta))
       (setf %:type :type-local-dimming-frame-end-info-meta
             %:next ,next
             ,@(when local-dimming-mode-p `(%:local-dimming-mode ,local-dimming-mode)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:local-dimming-frame-end-info-meta %struct-types%)
      ':type-local-dimming-frame-end-info-meta)

(defmacro with-spatial-anchor-persistence-name-msft ((pointer &key %slots
                                                                (name nil
                                                                      name-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-anchor-persistence-name-msft))
     (cffi:with-foreign-slots ((%:name)
                               ,pointer (:struct %:spatial-anchor-persistence-name-msft))
       (setf )
       (if (and ,name-p (not ,name))
           (setf %:name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:spatial-anchor-persistence-name-msft) '%:name)
            %:+max-spatial-anchor-name-size-msft+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-spatial-anchor-persistence-info-msft ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (spatial-anchor-persistence-name
                                                                 nil
                                                                 spatial-anchor-persistence-name-p)
                                                                (spatial-anchor
                                                                 nil
                                                                 spatial-anchor-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-anchor-persistence-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:spatial-anchor-persistence-name
                                %:spatial-anchor)
                               ,pointer (:struct %:spatial-anchor-persistence-info-msft))
       (setf %:type :type-spatial-anchor-persistence-info-msft
             %:next ,next
             ,@(when spatial-anchor-persistence-name-p `(%:spatial-anchor-persistence-name ,spatial-anchor-persistence-name))
             ,@(when spatial-anchor-p `(%:spatial-anchor ,spatial-anchor)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-anchor-persistence-info-msft %struct-types%)
      ':type-spatial-anchor-persistence-info-msft)

(defmacro with-spatial-anchor-from-persisted-anchor-create-info-msft ((pointer &key
                                                                                 %slots
                                                                                 (next
                                                                                  '(cffi:null-pointer))
                                                                                 (spatial-anchor-store
                                                                                  nil
                                                                                  spatial-anchor-store-p)
                                                                                 (spatial-anchor-persistence-name
                                                                                  nil
                                                                                  spatial-anchor-persistence-name-p))
                                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:spatial-anchor-from-persisted-anchor-create-info-msft))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:spatial-anchor-store
                                %:spatial-anchor-persistence-name)
                               ,pointer (:struct %:spatial-anchor-from-persisted-anchor-create-info-msft))
       (setf %:type :type-spatial-anchor-from-persisted-anchor-create-info-msft
             %:next ,next
             ,@(when spatial-anchor-store-p `(%:spatial-anchor-store ,spatial-anchor-store))
             ,@(when spatial-anchor-persistence-name-p `(%:spatial-anchor-persistence-name ,spatial-anchor-persistence-name)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:spatial-anchor-from-persisted-anchor-create-info-msft %struct-types%)
      ':type-spatial-anchor-from-persisted-anchor-create-info-msft)

(defmacro with-facial-tracker-create-info-htc ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (facial-tracking-type
                                                           nil
                                                           facial-tracking-type-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:facial-tracker-create-info-htc))
     (cffi:with-foreign-slots ((%:type %:next %:facial-tracking-type)
                               ,pointer (:struct %:facial-tracker-create-info-htc))
       (setf %:type :type-facial-tracker-create-info-htc
             %:next ,next
             ,@(when facial-tracking-type-p `(%:facial-tracking-type ,facial-tracking-type)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:facial-tracker-create-info-htc %struct-types%)
      ':type-facial-tracker-create-info-htc)

(defmacro with-system-facial-tracking-properties-htc ((pointer &key %slots
                                                                 (next
                                                                  '(cffi:null-pointer))
                                                                 (support-eye-facial-tracking
                                                                  nil
                                                                  support-eye-facial-tracking-p)
                                                                 (support-lip-facial-tracking
                                                                  nil
                                                                  support-lip-facial-tracking-p))
                                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-facial-tracking-properties-htc))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:support-eye-facial-tracking
                                %:support-lip-facial-tracking)
                               ,pointer (:struct %:system-facial-tracking-properties-htc))
       (setf %:type :type-system-facial-tracking-properties-htc
             %:next ,next
             ,@(when support-eye-facial-tracking-p `(%:support-eye-facial-tracking ,support-eye-facial-tracking))
             ,@(when support-lip-facial-tracking-p `(%:support-lip-facial-tracking ,support-lip-facial-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-facial-tracking-properties-htc %struct-types%)
      ':type-system-facial-tracking-properties-htc)

(defmacro with-facial-expressions-htc ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (is-active nil is-active-p)
                                                  (sample-time nil
                                                   sample-time-p)
                                                  (expression-count nil
                                                   expression-count-p)
                                                  (expression-weightings nil
                                                   expression-weightings-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:facial-expressions-htc))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:is-active
                                %:sample-time
                                %:expression-count
                                %:expression-weightings)
                               ,pointer (:struct %:facial-expressions-htc))
       (setf %:type :type-facial-expressions-htc
             %:next ,next
             ,@(when is-active-p `(%:is-active ,is-active))
             ,@(when sample-time-p `(%:sample-time ,sample-time))
             ,@(when expression-count-p `(%:expression-count ,expression-count))
             ,@(when expression-weightings-p `(%:expression-weightings ,expression-weightings)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:facial-expressions-htc %struct-types%)
      ':type-facial-expressions-htc)

(defmacro with-passthrough-create-info-htc ((pointer &key %slots
                                                       (next
                                                        '(cffi:null-pointer))
                                                       (form nil form-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-create-info-htc))
     (cffi:with-foreign-slots ((%:type %:next %:form)
                               ,pointer (:struct %:passthrough-create-info-htc))
       (setf %:type :type-passthrough-create-info-htc
             %:next ,next
             ,@(when form-p `(%:form ,form)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-create-info-htc %struct-types%)
      ':type-passthrough-create-info-htc)

(defmacro with-passthrough-color-htc ((pointer &key %slots
                                                 (next '(cffi:null-pointer))
                                                 (alpha nil alpha-p))
                                      &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-color-htc))
     (cffi:with-foreign-slots ((%:type %:next %:alpha)
                               ,pointer (:struct %:passthrough-color-htc))
       (setf %:type :type-passthrough-color-htc
             %:next ,next
             ,@(when alpha-p `(%:alpha ,alpha)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-color-htc %struct-types%)
      ':type-passthrough-color-htc)

(defmacro with-passthrough-mesh-transform-info-htc ((pointer &key %slots
                                                               (next
                                                                '(cffi:null-pointer))
                                                               (vertex-count
                                                                nil
                                                                vertex-count-p)
                                                               (vertices nil
                                                                vertices-p)
                                                               (index-count nil
                                                                index-count-p)
                                                               (indices nil
                                                                indices-p)
                                                               (base-space nil
                                                                base-space-p)
                                                               (time
                                                                nil
                                                                time-p)
                                                               (pose nil
                                                                pose-p)
                                                               (scale nil
                                                                scale-p))
                                                    &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:passthrough-mesh-transform-info-htc))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:vertex-count
                                %:vertices
                                %:index-count
                                %:indices
                                %:base-space
                                %:time
                                %:pose
                                %:scale)
                               ,pointer (:struct %:passthrough-mesh-transform-info-htc))
       (setf %:type :type-passthrough-mesh-transform-info-htc
             %:next ,next
             ,@(when vertex-count-p `(%:vertex-count ,vertex-count))
             ,@(when vertices-p `(%:vertices ,vertices))
             ,@(when index-count-p `(%:index-count ,index-count))
             ,@(when indices-p `(%:indices ,indices))
             ,@(when base-space-p `(%:base-space ,base-space))
             ,@(when time-p `(%:time ,time))
             ,@(when pose-p `(%:pose ,pose))
             ,@(when scale-p `(%:scale ,scale)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:passthrough-mesh-transform-info-htc %struct-types%)
      ':type-passthrough-mesh-transform-info-htc)

(defmacro with-composition-layer-passthrough-htc ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (layer-flags nil
                                                              layer-flags-p)
                                                             (space nil
                                                              space-p)
                                                             (passthrough nil
                                                              passthrough-p)
                                                             (color nil
                                                              color-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-passthrough-htc))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:space
                                %:passthrough
                                %:color)
                               ,pointer (:struct %:composition-layer-passthrough-htc))
       (setf %:type :type-composition-layer-passthrough-htc
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when space-p `(%:space ,space))
             ,@(when passthrough-p `(%:passthrough ,passthrough))
             ,@(when color-p `(%:color ,color)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-passthrough-htc %struct-types%)
      ':type-composition-layer-passthrough-htc)

(defmacro with-vive-tracker-paths-htcx ((pointer &key %slots
                                                   (next '(cffi:null-pointer))
                                                   (persistent-path nil
                                                    persistent-path-p)
                                                   (role-path nil role-path-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:vive-tracker-paths-htcx))
     (cffi:with-foreign-slots ((%:type %:next %:persistent-path %:role-path)
                               ,pointer (:struct %:vive-tracker-paths-htcx))
       (setf %:type :type-vive-tracker-paths-htcx
             %:next ,next
             ,@(when persistent-path-p `(%:persistent-path ,persistent-path))
             ,@(when role-path-p `(%:role-path ,role-path)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:vive-tracker-paths-htcx %struct-types%)
      ':type-vive-tracker-paths-htcx)

(defmacro with-event-data-vive-tracker-connected-htcx ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (paths nil
                                                                   paths-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-vive-tracker-connected-htcx))
     (cffi:with-foreign-slots ((%:type %:next %:paths)
                               ,pointer (:struct %:event-data-vive-tracker-connected-htcx))
       (setf %:type :type-event-data-vive-tracker-connected-htcx
             %:next ,next
             ,@(when paths-p `(%:paths ,paths)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-vive-tracker-connected-htcx %struct-types%)
      ':type-event-data-vive-tracker-connected-htcx)

(defmacro with-composition-layer-space-warp-info-fb ((pointer &key %slots
                                                                (next
                                                                 '(cffi:null-pointer))
                                                                (layer-flags
                                                                 nil
                                                                 layer-flags-p)
                                                                (motion-vector-sub-image
                                                                 nil
                                                                 motion-vector-sub-image-p)
                                                                (app-space-delta-pose
                                                                 nil
                                                                 app-space-delta-pose-p)
                                                                (depth-sub-image
                                                                 nil
                                                                 depth-sub-image-p)
                                                                (min-depth nil
                                                                 min-depth-p)
                                                                (max-depth nil
                                                                 max-depth-p)
                                                                (near-z nil
                                                                 near-z-p)
                                                                (far-z nil
                                                                 far-z-p))
                                                     &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-space-warp-info-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:layer-flags
                                %:motion-vector-sub-image
                                %:app-space-delta-pose
                                %:depth-sub-image
                                %:min-depth
                                %:max-depth
                                %:near-z
                                %:far-z)
                               ,pointer (:struct %:composition-layer-space-warp-info-fb))
       (setf %:type :type-composition-layer-space-warp-info-fb
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags))
             ,@(when motion-vector-sub-image-p `(%:motion-vector-sub-image ,motion-vector-sub-image))
             ,@(when app-space-delta-pose-p `(%:app-space-delta-pose ,app-space-delta-pose))
             ,@(when depth-sub-image-p `(%:depth-sub-image ,depth-sub-image))
             ,@(when min-depth-p `(%:min-depth ,min-depth))
             ,@(when max-depth-p `(%:max-depth ,max-depth))
             ,@(when near-z-p `(%:near-z ,near-z))
             ,@(when far-z-p `(%:far-z ,far-z)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-space-warp-info-fb %struct-types%)
      ':type-composition-layer-space-warp-info-fb)

(defmacro with-system-space-warp-properties-fb ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (recommended-motion-vector-image-rect-width
                                                            nil
                                                            recommended-motion-vector-image-rect-width-p)
                                                           (recommended-motion-vector-image-rect-height
                                                            nil
                                                            recommended-motion-vector-image-rect-height-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-space-warp-properties-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:recommended-motion-vector-image-rect-width
                                %:recommended-motion-vector-image-rect-height)
                               ,pointer (:struct %:system-space-warp-properties-fb))
       (setf %:type :type-system-space-warp-properties-fb
             %:next ,next
             ,@(when recommended-motion-vector-image-rect-width-p `(%:recommended-motion-vector-image-rect-width ,recommended-motion-vector-image-rect-width))
             ,@(when recommended-motion-vector-image-rect-height-p `(%:recommended-motion-vector-image-rect-height ,recommended-motion-vector-image-rect-height)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-space-warp-properties-fb %struct-types%)
      ':type-system-space-warp-properties-fb)

(defmacro with-system-marker-tracking-properties-varjo ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (supports-marker-tracking
                                                                    nil
                                                                    supports-marker-tracking-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-marker-tracking-properties-varjo))
     (cffi:with-foreign-slots ((%:type %:next %:supports-marker-tracking)
                               ,pointer (:struct %:system-marker-tracking-properties-varjo))
       (setf %:type :type-system-marker-tracking-properties-varjo
             %:next ,next
             ,@(when supports-marker-tracking-p `(%:supports-marker-tracking ,supports-marker-tracking)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-marker-tracking-properties-varjo %struct-types%)
      ':type-system-marker-tracking-properties-varjo)

(defmacro with-event-data-marker-tracking-update-varjo ((pointer &key %slots
                                                                   (next
                                                                    '(cffi:null-pointer))
                                                                   (marker-id
                                                                    nil
                                                                    marker-id-p)
                                                                   (is-active
                                                                    nil
                                                                    is-active-p)
                                                                   (is-predicted
                                                                    nil
                                                                    is-predicted-p)
                                                                   (time
                                                                    nil
                                                                    time-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:event-data-marker-tracking-update-varjo))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:marker-id
                                %:is-active
                                %:is-predicted
                                %:time)
                               ,pointer (:struct %:event-data-marker-tracking-update-varjo))
       (setf %:type :type-event-data-marker-tracking-update-varjo
             %:next ,next
             ,@(when marker-id-p `(%:marker-id ,marker-id))
             ,@(when is-active-p `(%:is-active ,is-active))
             ,@(when is-predicted-p `(%:is-predicted ,is-predicted))
             ,@(when time-p `(%:time ,time)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:event-data-marker-tracking-update-varjo %struct-types%)
      ':type-event-data-marker-tracking-update-varjo)

(defmacro with-marker-space-create-info-varjo ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (marker-id nil
                                                           marker-id-p)
                                                          (pose-in-marker-space
                                                           nil
                                                           pose-in-marker-space-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:marker-space-create-info-varjo))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:marker-id
                                %:pose-in-marker-space)
                               ,pointer (:struct %:marker-space-create-info-varjo))
       (setf %:type :type-marker-space-create-info-varjo
             %:next ,next
             ,@(when marker-id-p `(%:marker-id ,marker-id))
             ,@(when pose-in-marker-space-p `(%:pose-in-marker-space ,pose-in-marker-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:marker-space-create-info-varjo %struct-types%)
      ':type-marker-space-create-info-varjo)

(defmacro with-uuid-ext ((pointer &key %slots (data nil data-p)) &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:uuid-ext))
     (cffi:with-foreign-slots ((%:data)
                               ,pointer (:struct %:uuid-ext))
       (setf ,@(when data-p `(%:data ,data)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-global-dimmer-frame-end-info-ml ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (dimmer-value nil
                                                            dimmer-value-p)
                                                           (flags nil flags-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:global-dimmer-frame-end-info-ml))
     (cffi:with-foreign-slots ((%:type %:next %:dimmer-value %:flags)
                               ,pointer (:struct %:global-dimmer-frame-end-info-ml))
       (setf %:type :type-global-dimmer-frame-end-info-ml
             %:next ,next
             ,@(when dimmer-value-p `(%:dimmer-value ,dimmer-value))
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:global-dimmer-frame-end-info-ml %struct-types%)
      ':type-global-dimmer-frame-end-info-ml)

(defmacro with-digital-lens-control-almalence ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (flags nil flags-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:digital-lens-control-almalence))
     (cffi:with-foreign-slots ((%:type %:next %:flags)
                               ,pointer (:struct %:digital-lens-control-almalence))
       (setf %:type :type-digital-lens-control-almalence
             %:next ,next
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:digital-lens-control-almalence %struct-types%)
      ':type-digital-lens-control-almalence)

(defmacro with-composition-layer-settings-fb ((pointer &key %slots
                                                         (next
                                                          '(cffi:null-pointer))
                                                         (layer-flags nil
                                                          layer-flags-p))
                                              &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-settings-fb))
     (cffi:with-foreign-slots ((%:type %:next %:layer-flags)
                               ,pointer (:struct %:composition-layer-settings-fb))
       (setf %:type :type-composition-layer-settings-fb
             %:next ,next
             ,@(when layer-flags-p `(%:layer-flags ,layer-flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-settings-fb %struct-types%)
      ':type-composition-layer-settings-fb)

(defmacro with-external-camera-intrinsics-oculus ((pointer &key %slots
                                                             (last-change-time
                                                              nil
                                                              last-change-time-p)
                                                             (fov nil fov-p)
                                                             (virtual-near-plane-distance
                                                              nil
                                                              virtual-near-plane-distance-p)
                                                             (virtual-far-plane-distance
                                                              nil
                                                              virtual-far-plane-distance-p)
                                                             (image-sensor-pixel-resolution
                                                              nil
                                                              image-sensor-pixel-resolution-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:external-camera-intrinsics-oculus))
     (cffi:with-foreign-slots ((%:last-change-time
                                %:fov
                                %:virtual-near-plane-distance
                                %:virtual-far-plane-distance
                                %:image-sensor-pixel-resolution)
                               ,pointer (:struct %:external-camera-intrinsics-oculus))
       (setf ,@(when last-change-time-p `(%:last-change-time ,last-change-time))
             ,@(when fov-p `(%:fov ,fov))
             ,@(when virtual-near-plane-distance-p `(%:virtual-near-plane-distance ,virtual-near-plane-distance))
             ,@(when virtual-far-plane-distance-p `(%:virtual-far-plane-distance ,virtual-far-plane-distance))
             ,@(when image-sensor-pixel-resolution-p `(%:image-sensor-pixel-resolution ,image-sensor-pixel-resolution)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-external-camera-extrinsics-oculus ((pointer &key %slots
                                                             (last-change-time
                                                              nil
                                                              last-change-time-p)
                                                             (camera-status-flags
                                                              nil
                                                              camera-status-flags-p)
                                                             (attached-to-device
                                                              nil
                                                              attached-to-device-p)
                                                             (relative-pose nil
                                                              relative-pose-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:external-camera-extrinsics-oculus))
     (cffi:with-foreign-slots ((%:last-change-time
                                %:camera-status-flags
                                %:attached-to-device
                                %:relative-pose)
                               ,pointer (:struct %:external-camera-extrinsics-oculus))
       (setf ,@(when last-change-time-p `(%:last-change-time ,last-change-time))
             ,@(when camera-status-flags-p `(%:camera-status-flags ,camera-status-flags))
             ,@(when attached-to-device-p `(%:attached-to-device ,attached-to-device))
             ,@(when relative-pose-p `(%:relative-pose ,relative-pose)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-external-camera-oculus ((pointer &key %slots
                                                  (next '(cffi:null-pointer))
                                                  (name nil name-p)
                                                  (intrinsics nil intrinsics-p)
                                                  (extrinsics nil extrinsics-p))
                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:external-camera-oculus))
     (cffi:with-foreign-slots ((%:type %:next %:name %:intrinsics %:extrinsics)
                               ,pointer (:struct %:external-camera-oculus))
       (setf %:type :type-external-camera-oculus
             %:next ,next
             ,@(when intrinsics-p `(%:intrinsics ,intrinsics))
             ,@(when extrinsics-p `(%:extrinsics ,extrinsics)))
       (if (and ,name-p (not ,name))
           (setf %:name (cffi:null-pointer))
           (cffi:lisp-string-to-foreign (or ,name "")
            (cffi:foreign-slot-pointer ,pointer
             '(:struct %:external-camera-oculus) '%:name)
            %:+max-external-camera-name-size-oculus+ :encoding :utf-8))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:external-camera-oculus %struct-types%)
      ':type-external-camera-oculus)

(defmacro with-performance-metrics-state-meta ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (enabled nil
                                                           enabled-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:performance-metrics-state-meta))
     (cffi:with-foreign-slots ((%:type %:next %:enabled)
                               ,pointer (:struct %:performance-metrics-state-meta))
       (setf %:type :type-performance-metrics-state-meta
             %:next ,next
             ,@(when enabled-p `(%:enabled ,enabled)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:performance-metrics-state-meta %struct-types%)
      ':type-performance-metrics-state-meta)

(defmacro with-performance-metrics-counter-meta ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (counter-flags nil
                                                             counter-flags-p)
                                                            (counter-unit nil
                                                             counter-unit-p)
                                                            (uint-value nil
                                                             uint-value-p)
                                                            (float-value nil
                                                             float-value-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:performance-metrics-counter-meta))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:counter-flags
                                %:counter-unit
                                %:uint-value
                                %:float-value)
                               ,pointer (:struct %:performance-metrics-counter-meta))
       (setf %:type :type-performance-metrics-counter-meta
             %:next ,next
             ,@(when counter-flags-p `(%:counter-flags ,counter-flags))
             ,@(when counter-unit-p `(%:counter-unit ,counter-unit))
             ,@(when uint-value-p `(%:uint-value ,uint-value))
             ,@(when float-value-p `(%:float-value ,float-value)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:performance-metrics-counter-meta %struct-types%)
      ':type-performance-metrics-counter-meta)

(defmacro with-system-headset-id-properties-meta ((pointer &key %slots
                                                             (next
                                                              '(cffi:null-pointer))
                                                             (id nil id-p))
                                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-headset-id-properties-meta))
     (cffi:with-foreign-slots ((%:type %:next %:id)
                               ,pointer (:struct %:system-headset-id-properties-meta))
       (setf %:type :type-system-headset-id-properties-meta
             %:next ,next
             ,@(when id-p `(%:id ,id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-headset-id-properties-meta %struct-types%)
      ':type-system-headset-id-properties-meta)

(defmacro with-foveation-apply-info-htc ((pointer &key %slots
                                                    (next '(cffi:null-pointer))
                                                    (mode nil mode-p)
                                                    (sub-image-count nil
                                                     sub-image-count-p)
                                                    (sub-images nil
                                                     sub-images-p))
                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-apply-info-htc))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:mode
                                %:sub-image-count
                                %:sub-images)
                               ,pointer (:struct %:foveation-apply-info-htc))
       (setf %:type :type-foveation-apply-info-htc
             %:next ,next
             ,@(when mode-p `(%:mode ,mode))
             ,@(when sub-image-count-p `(%:sub-image-count ,sub-image-count))
             ,@(when sub-images-p `(%:sub-images ,sub-images)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-apply-info-htc %struct-types%)
      ':type-foveation-apply-info-htc)

(defmacro with-foveation-configuration-htc ((pointer &key %slots
                                                       (level nil level-p)
                                                       (clear-fov-degree nil
                                                        clear-fov-degree-p)
                                                       (focal-center-offset nil
                                                        focal-center-offset-p))
                                            &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-configuration-htc))
     (cffi:with-foreign-slots ((%:level
                                %:clear-fov-degree
                                %:focal-center-offset)
                               ,pointer (:struct %:foveation-configuration-htc))
       (setf ,@(when level-p `(%:level ,level))
             ,@(when clear-fov-degree-p `(%:clear-fov-degree ,clear-fov-degree))
             ,@(when focal-center-offset-p `(%:focal-center-offset ,focal-center-offset)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-foveation-dynamic-mode-info-htc ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (dynamic-flags nil
                                                            dynamic-flags-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-dynamic-mode-info-htc))
     (cffi:with-foreign-slots ((%:type %:next %:dynamic-flags)
                               ,pointer (:struct %:foveation-dynamic-mode-info-htc))
       (setf %:type :type-foveation-dynamic-mode-info-htc
             %:next ,next
             ,@(when dynamic-flags-p `(%:dynamic-flags ,dynamic-flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-dynamic-mode-info-htc %struct-types%)
      ':type-foveation-dynamic-mode-info-htc)

(defmacro with-foveation-custom-mode-info-htc ((pointer &key %slots
                                                          (next
                                                           '(cffi:null-pointer))
                                                          (config-count nil
                                                           config-count-p)
                                                          (configs nil
                                                           configs-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:foveation-custom-mode-info-htc))
     (cffi:with-foreign-slots ((%:type %:next %:config-count %:configs)
                               ,pointer (:struct %:foveation-custom-mode-info-htc))
       (setf %:type :type-foveation-custom-mode-info-htc
             %:next ,next
             ,@(when config-count-p `(%:config-count ,config-count))
             ,@(when configs-p `(%:configs ,configs)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:foveation-custom-mode-info-htc %struct-types%)
      ':type-foveation-custom-mode-info-htc)

(defmacro with-active-action-set-priorities-ext ((pointer &key %slots
                                                            (next
                                                             '(cffi:null-pointer))
                                                            (action-set-priority-count
                                                             nil
                                                             action-set-priority-count-p)
                                                            (action-set-priorities
                                                             nil
                                                             action-set-priorities-p))
                                                 &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:active-action-set-priorities-ext))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:action-set-priority-count
                                %:action-set-priorities)
                               ,pointer (:struct %:active-action-set-priorities-ext))
       (setf %:type :type-active-action-set-priorities-ext
             %:next ,next
             ,@(when action-set-priority-count-p `(%:action-set-priority-count ,action-set-priority-count))
             ,@(when action-set-priorities-p `(%:action-set-priorities ,action-set-priorities)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:active-action-set-priorities-ext %struct-types%)
      ':type-active-action-set-priorities-ext)

(defmacro with-active-action-set-priority-ext ((pointer &key %slots
                                                          (action-set nil
                                                           action-set-p)
                                                          (priority-override
                                                           nil
                                                           priority-override-p))
                                               &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:active-action-set-priority-ext))
     (cffi:with-foreign-slots ((%:action-set %:priority-override)
                               ,pointer (:struct %:active-action-set-priority-ext))
       (setf ,@(when action-set-p `(%:action-set ,action-set))
             ,@(when priority-override-p `(%:priority-override ,priority-override)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))

(defmacro with-composition-layer-depth-test-fb ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (depth-mask nil
                                                            depth-mask-p)
                                                           (compare-op nil
                                                            compare-op-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:composition-layer-depth-test-fb))
     (cffi:with-foreign-slots ((%:type %:next %:depth-mask %:compare-op)
                               ,pointer (:struct %:composition-layer-depth-test-fb))
       (setf %:type :type-composition-layer-depth-test-fb
             %:next ,next
             ,@(when depth-mask-p `(%:depth-mask ,depth-mask))
             ,@(when compare-op-p `(%:compare-op ,compare-op)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:composition-layer-depth-test-fb %struct-types%)
      ':type-composition-layer-depth-test-fb)

(defmacro with-coordinate-space-create-info-ml ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (cfuid nil cfuid-p)
                                                           (pose-in-coordinate-space
                                                            nil
                                                            pose-in-coordinate-space-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:coordinate-space-create-info-ml))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:cfuid
                                %:pose-in-coordinate-space)
                               ,pointer (:struct %:coordinate-space-create-info-ml))
       (setf %:type :type-coordinate-space-create-info-ml
             %:next ,next
             ,@(when cfuid-p `(%:cfuid ,cfuid))
             ,@(when pose-in-coordinate-space-p `(%:pose-in-coordinate-space ,pose-in-coordinate-space)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:coordinate-space-create-info-ml %struct-types%)
      ':type-coordinate-space-create-info-ml)

(defmacro with-frame-end-info-ml ((pointer &key %slots
                                             (next '(cffi:null-pointer))
                                             (focus-distance nil
                                              focus-distance-p)
                                             (flags nil flags-p))
                                  &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:frame-end-info-ml))
     (cffi:with-foreign-slots ((%:type %:next %:focus-distance %:flags)
                               ,pointer (:struct %:frame-end-info-ml))
       (setf %:type :type-frame-end-info-ml
             %:next ,next
             ,@(when focus-distance-p `(%:focus-distance ,focus-distance))
             ,@(when flags-p `(%:flags ,flags)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:frame-end-info-ml %struct-types%)
      ':type-frame-end-info-ml)

(defmacro with-haptic-amplitude-envelope-vibration-fb ((pointer &key %slots
                                                                  (next
                                                                   '(cffi:null-pointer))
                                                                  (duration nil
                                                                   duration-p)
                                                                  (amplitude-count
                                                                   nil
                                                                   amplitude-count-p)
                                                                  (amplitudes
                                                                   nil
                                                                   amplitudes-p))
                                                       &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:haptic-amplitude-envelope-vibration-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:duration
                                %:amplitude-count
                                %:amplitudes)
                               ,pointer (:struct %:haptic-amplitude-envelope-vibration-fb))
       (setf %:type :type-haptic-amplitude-envelope-vibration-fb
             %:next ,next
             ,@(when duration-p `(%:duration ,duration))
             ,@(when amplitude-count-p `(%:amplitude-count ,amplitude-count))
             ,@(when amplitudes-p `(%:amplitudes ,amplitudes)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:haptic-amplitude-envelope-vibration-fb %struct-types%)
      ':type-haptic-amplitude-envelope-vibration-fb)

(defmacro with-haptic-pcm-vibration-fb ((pointer &key %slots
                                                   (next '(cffi:null-pointer))
                                                   (buffer-size nil
                                                    buffer-size-p)
                                                   (buffer nil buffer-p)
                                                   (sample-rate nil
                                                    sample-rate-p)
                                                   (append nil append-p)
                                                   (samples-consumed nil
                                                    samples-consumed-p))
                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:haptic-pcm-vibration-fb))
     (cffi:with-foreign-slots ((%:type
                                %:next
                                %:buffer-size
                                %:buffer
                                %:sample-rate
                                %:append
                                %:samples-consumed)
                               ,pointer (:struct %:haptic-pcm-vibration-fb))
       (setf %:type :type-haptic-pcm-vibration-fb
             %:next ,next
             ,@(when buffer-size-p `(%:buffer-size ,buffer-size))
             ,@(when buffer-p `(%:buffer ,buffer))
             ,@(when sample-rate-p `(%:sample-rate ,sample-rate))
             ,@(when append-p `(%:append ,append))
             ,@(when samples-consumed-p `(%:samples-consumed ,samples-consumed)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:haptic-pcm-vibration-fb %struct-types%)
      ':type-haptic-pcm-vibration-fb)

(defmacro with-device-pcm-sample-rate-state-fb ((pointer &key %slots
                                                           (next
                                                            '(cffi:null-pointer))
                                                           (sample-rate nil
                                                            sample-rate-p))
                                                &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:device-pcm-sample-rate-state-fb))
     (cffi:with-foreign-slots ((%:type %:next %:sample-rate)
                               ,pointer (:struct %:device-pcm-sample-rate-state-fb))
       (setf %:type :type-device-pcm-sample-rate-state-fb
             %:next ,next
             ,@(when sample-rate-p `(%:sample-rate ,sample-rate)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:device-pcm-sample-rate-state-fb %struct-types%)
      ':type-device-pcm-sample-rate-state-fb)

(defmacro with-space-user-create-info-fb ((pointer &key %slots
                                                     (next
                                                      '(cffi:null-pointer))
                                                     (user-id nil user-id-p))
                                          &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:space-user-create-info-fb))
     (cffi:with-foreign-slots ((%:type %:next %:user-id)
                               ,pointer (:struct %:space-user-create-info-fb))
       (setf %:type :type-space-user-create-info-fb
             %:next ,next
             ,@(when user-id-p `(%:user-id ,user-id)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:space-user-create-info-fb %struct-types%)
      ':type-space-user-create-info-fb)

(defmacro with-system-force-feedback-curl-properties-mndx ((pointer &key %slots
                                                                      (next
                                                                       '(cffi:null-pointer))
                                                                      (supports-force-feedback-curl
                                                                       nil
                                                                       supports-force-feedback-curl-p))
                                                           &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:system-force-feedback-curl-properties-mndx))
     (cffi:with-foreign-slots ((%:type %:next %:supports-force-feedback-curl)
                               ,pointer (:struct %:system-force-feedback-curl-properties-mndx))
       (setf %:type :type-system-force-feedback-curl-properties-mndx
             %:next ,next
             ,@(when supports-force-feedback-curl-p `(%:supports-force-feedback-curl ,supports-force-feedback-curl)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:system-force-feedback-curl-properties-mndx %struct-types%)
      ':type-system-force-feedback-curl-properties-mndx)

(defmacro with-force-feedback-curl-apply-locations-mndx ((pointer &key %slots
                                                                    (next
                                                                     '(cffi:null-pointer))
                                                                    (location-count
                                                                     nil
                                                                     location-count-p)
                                                                    (locations
                                                                     nil
                                                                     locations-p))
                                                         &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:force-feedback-curl-apply-locations-mndx))
     (cffi:with-foreign-slots ((%:type %:next %:location-count %:locations)
                               ,pointer (:struct %:force-feedback-curl-apply-locations-mndx))
       (setf %:type :type-force-feedback-curl-apply-locations-mndx
             %:next ,next
             ,@(when location-count-p `(%:location-count ,location-count))
             ,@(when locations-p `(%:locations ,locations)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
(setf (gethash '%:force-feedback-curl-apply-locations-mndx %struct-types%)
      ':type-force-feedback-curl-apply-locations-mndx)

(defmacro with-force-feedback-curl-apply-location-mndx ((pointer &key %slots
                                                                   (location
                                                                    nil
                                                                    location-p)
                                                                   (value nil
                                                                          value-p))
                                                        &body body)
  `(cffi:with-foreign-object (,pointer '(:struct %:force-feedback-curl-apply-location-mndx))
     (cffi:with-foreign-slots ((%:location %:value)
                               ,pointer (:struct %:force-feedback-curl-apply-location-mndx))
       (setf ,@(when location-p `(%:location ,location))
             ,@(when value-p `(%:value ,value)))
       ,@(if %slots body nil))
     ,@(if %slots nil body)))
