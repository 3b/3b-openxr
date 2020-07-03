#++(delete-package '#:3b-openxr-bindings)
(in-package #:3b-openxr-bindings)

(defctype xr-handle :uint64)
(defctype atom :uint64)

(defctype a-native-window :pointer) ;; "android/native_window.h"
(defctype j-object :pointer)              ;; "Jni.h"

;; not sure about type of this
(defctype cgl-context-obj (:pointer :void)) ;;"CL/cl_gl_ext.h"
(defctype d3d-feature-level :int) ;;"d3dcommon.h" (a c enum)
;; not sure about type of d3d interfaces?
(defctype i-d3d11-device (:pointer :void)) ;;"D3D11.h"
(defctype i-d3d11-texture-2d (:pointer :void)) ;;"D3D11.h"
(defctype i-d3d12-command-queue (:pointer :void)) ;;"D3D12.h"
(defctype i-d3d12-device (:pointer :void))     ;;"D3D12.h"
(defctype i-d3d12-resource (:pointer :void)) ;;"D3D12.h"
;; todo: these can be enums too
(defctype egl-display (:pointer :void)) ;;"EGL/egl.h"
(defctype egl-config (:pointer :void))   ;;"EGL/egl.h"
(defctype egl-context (:pointer :void)) ;;"EGL/egl.h"
(defctype pfn-egl-get-proc-address-proc  (:pointer :void)) ;; "EGL/egl.h"
(defctype glx-drawable :unsigned-long)       ;;XID "GL/glxext.h"
(defctype glx-fb-config :pointer)            ;;"GL/glxext.h"
(defctype glx-context :unsigned-long)        ;;XID "GL/glxext.h"

(defctype wl-display :pointer)           ;;"wayland-client.h"

(defctype hglrc :pointer)
(defctype handle hglrc)              ;;"GL/wglext.h"

(defctype hdc handle)                          ;;"windows.h"
(defctype luid :pointer)                       ;;"windows.h"
(defctype large-integer :int64)      ;;"windows.h"

(defctype display :pointer)                  ;;"X11/Xlib.h"
(defctype visual-id :unsigned-long)       ;;"X11/Xlib.h"
(defctype window :unsigned-long)                       ;; XID"X11/Xlib.h"

(defctype xcb-glx-fbconfig-t :uint32) ;;"xcb/glx.h"
(defctype xcb-glx-drawable-t :uint32) ;;"xcb/glx.h"
(defctype xcb-glx-context-t :uint32)   ;;"xcb/glx.h"
(defctype xcb-connection-t :uint32)     ;;"xcb/xcb.h"
(defctype xcb-visualid-t :uint32)         ;;"xcb/xcb.h"
(defctype xcb-window-t :uint32)             ;;"xcb/xcb.h"
(defctype vk-device :pointer)  ;;"vulkan/vulkan.h"
(defctype vk-format :pointer)  ;;"vulkan/vulkan.h"
(defctype vk-image :pointer)    ;;"vulkan/vulkan.h"
(defctype vk-image :pointer) ;;"vulkan/vulkan.h"
(defctype vk-physical-device :pointer) ;;"vulkan/vulkan.h"

(defctype timespec :void) ;;"time.h" ;; only used through pointer?

(defctype void-function :pointer)
