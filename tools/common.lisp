#++
(ql:quickload '3b-openxr-generator)
(defpackage #:3b-openxr-parse-spec2
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2))
  (:shadow #:type #:number #:require #:member #:values))

(in-package #:3b-openxr-parse-spec2)

;; we don't always have enough info to translate an enum name within a
;; node, so store them as we see them
(defparameter *known-enum-names* (make-hash-table :test 'equal))

(defparameter *platform-types*
  (alexandria:plist-hash-table
   '("void" :void
     "char" :char
     "float" :float
     "int8_t" :int8
     "uint8_t" :uint8
     "int16_t" :int16
     "uint16_t" :uint16
     "int32_t" :int32
     "uint32_t" :uint32
     "int64_t" :int64
     "uint64_t" :uint64
     "size_t" size-t
     "uintptr_t" uintptr-t
     "atom" :uint64
     ;; fixme: figure out actual size of wchar_t portably?
     "wchar_t" #+windows :uint16 #-windows :uint32
     ;; xr handle is (:pointer (:struct object_x)) on 64bit systems,
     ;; uint64 otherwise... just using uint64 for now
     "xr-handle" :uint64)
   :test 'equal))
(defparameter *os-types*
  (alexandria:plist-hash-table
   '("ANativeWindow" (:opaque a-native-window) ;; "android/native_window.h"
     "jobject" (:opaque j-object)              ;; "jni.h"
     ;; not sure about type of this
     "CGLContextObj" ((:pointer :void) cgl-context-obj) ;;"CL/cl_gl_ext.h"
     "D3D_FEATURE_LEVEL" (:int d3d-feature-level) ;;"d3dcommon.h" (a c enum)
     ;; not sure about type of d3d interfaces?
     "ID3D11Device" ((:pointer :void) i-d3d11-device) ;;"D3D11.h"
     "ID3D11Texture2D" ((:pointer :void) i-d3d11-texture-2d) ;;"D3D11.h"
     "ID3D12CommandQueue" ((:pointer :void) i-d3d12-command-queue) ;;"D3D12.h"
     "ID3D12Device" ((:pointer :void) i-d3d12-device)     ;;"D3D12.h"
     "ID3D12Resource" ((:pointer :void) i-d3d12-resource) ;;"D3D12.h"
     ;; todo: these can be enums too
     "EGLDisplay" ((:pointer :void) egl-display) ;;"EGL/egl.h"
     "EGLConfig" ((:pointer :void) egl-config)   ;;"EGL/egl.h"
     "EGLContext" ((:pointer :void) egl-context) ;;"EGL/egl.h"
     "EGLenum" (:unsigned-int egl-enum)          ;;"EGL/egl.h"
     "PFNEGLGETPROCADDRESSPROC" ((:pointer :void) pfn-egl-get-proc-address-proc) ;; "EGL/egl.h"
     "GLXFBConfig" (:opaque glx-fb-config)  ;;"GL/glxext.h"
     "GLXDrawable" (xid glx-drawable)       ;;"GL/glxext.h"
     "GLXContext" (xid glx-context)         ;;"GL/glxext.h"
     "HGLRC" (handle hglrc)                 ;;"GL/wglext.h"
     "wl_display" (:opaque wl-display)      ;;"wayland-client.h"
     "HDC" (handle hdc)                     ;;"windows.h"
     "LUID" (:opaque luid)                  ;;"windows.h"
     "LARGE_INTEGER" (:int64 large-integer) ;;"windows.h"
     "IUnknown" (:opaque i-unknown)         ;;"unknwn.h"
     "Display" (:opaque display)            ;;"X11/Xlib.h"
     "VisualID" (:unsigned-long visual-id)  ;;"X11/Xlib.h"
     "Window" (xid window)                  ;;"X11/Xlib.h"
     "xcb_glx_fbconfig_t" (:uint32 xcb-glx-fbconfig-t) ;;"xcb/glx.h"
     "xcb_glx_drawable_t" (:uint32 xcb-glx-drawable-t) ;;"xcb/glx.h"
     "xcb_glx_context_t" (:uint32 xcb-glx-context-t)   ;;"xcb/glx.h"
     "xcb_connection_t" (:uint32 xcb-connection-t)     ;;"xcb/xcb.h"
     "xcb_visualid_t" (:uint32 xcb-visualid-t)         ;;"xcb/xcb.h"
     "xcb_window_t" (:uint32 xcb-window-t)             ;;"xcb/xcb.h"
     "VkDevice" (:opaque vk-device)  ;;"vulkan/vulkan.h"
     "VkFormat" (:opaque vk-format)  ;;"vulkan/vulkan.h"
     "VkImage" (:opaque vk-image)    ;;"vulkan/vulkan.h"
     "VkInstance" (:opaque vk-image) ;;"vulkan/vulkan.h"
     "PFN_vkGetInstanceProcAddr" ((:pointer :opaque) vk-get-device-instance-proc-addr) ;; "vulkan/vulkan.h"
     "VkAllocationCallbacks" (:opaque vk-allocation-callbacks) ;;"vulkan/vulkan.h"
     "VkPhysicalDevice" (:opaque vk-physical-device) ;;"vulkan/vulkan.h"
     "VkComponentSwizzle" (:opaque vk-component-swizzle) ;; "vulkan/vulkan.h"
     "VkDeviceCreateInfo" (:opaque vk-device-create-info) ;; "vulkan/vulkan.h"
     "VkFilter" (:opaque vk-filter) ;; "vulkan/vulkan.h"
     "VkImageCreateFlags" (:opaque vk-image-create-flags) ;; "vulkan/vulkan.h"
     "VkImageUsageFlags" (:opaque vk-image-usage-flags) ;; "vulkan/vulkan.h"
     "VkInstanceCreateInfo" (:opaque vk-instance-create-info) ;; "vulkan/vulkan.h"
     "VkInstanceCreateFlags" (:opaque vk-instance-create-flags) ;; "vulkan/vulkan.h"
     "VkResult" (:opaque vk-result) ;; "vulkan/vulkan.h"
     "VkSamplerAddressMode" (:opaque vk-sampler-address-mode) ;; "vulkan/vulkan.h"
     "VkSamplerMipmapMode" (:opaque vk-sampler-mipmap-mode) ;; "vulkan/vulkan.h"

     "MLCoordinateFrameUID" (:opaque ml-coordinate-frame-uid) ;; "ml_coordinate_frame_uid.h"

     "timespec" ((:struct timespec) timespec) ;;"time.h"
     ;; not really os types, but needs defined similarly
     "PFN_xrDebugUtilsMessengerCallbackEXT" ((:pointer :opaque) pfn-debug-utils-messenger-callback-ext)
     "PFN_xrVoidFunction" ((:pointer :opaque) pfn-void-function))
   :test 'equal))

(defvar *old-names*
  (alexandria:plist-hash-table
   '("XrBool32" bool-32
     "XrFlags64" flags-64
     "XrTime" time
     "XrDuration" duration
     "XrVersion" version
     "XrPath" path
     "XrSystemId" system-id
     "XR_DEFINE_ATOM" atom
     "XR_DEFINE_HANDLE" xr-handle
     "openxr_platform_defines" openxr-platform-defines)
   :test 'equalp))



(defvar *ext-enum-base* 1000000000)
(defvar *ext-enum-block* 1000)

(defun ext-enum-value (ext dir value)
  (when (char= #\" (char value 0))
    (return-from ext-enum-value value))
  (let ((dir (make-keyword dir))
        (ext (parse-integer ext))
        (value (parse-integer value)))
    (ecase dir
      (:nil
       (+ *ext-enum-base*
          (* *ext-enum-block* (1- ext))
          value))
      (:-
       (- (+ *ext-enum-base*
             (* *ext-enum-block* (1- ext))
             value))))))


(defparameter *suffix-list* nil)
(defparameter *suffix-regex* nil)

(defun suffix-dashes (name)
  (unless *suffix-regex*
    (assert (>= (length *suffix-list*) 16))
    (setf *suffix-regex*
          (ppcre:create-scanner
           (format nil "(.)([1-4]?[if]~{|~a~})$"
                   (sort (copy-seq *suffix-list*) 'string<)))))
  (cl-ppcre:regex-replace-all *suffix-regex* name "\\1-\\2"))


(defun fix-api-names (name)
  (setf name (cl-ppcre:regex-replace-all "OpenGLES" name "Opengl-Es"))
  (setf name (cl-ppcre:regex-replace-all "(D3D11|D3D12|2D|3D)([a-zA-Z])"
                                         name
                                         (lambda (m r1 r2)
                                           (declare (ignore m))
                                           (concatenate 'string
                                                        r1
                                                        "-"
                                                        r2))
                                         :simple-calls t))
  (setf name (cl-ppcre:regex-replace-all "(Win32)([a-zA-Z])?"
                                         name
                                         (lambda (m r1 r2)
                                           (declare (ignore m))
                                           (concatenate 'string
                                                        r1
                                                        "-"
                                                        r2))
                                         :simple-calls t))

  (cl-ppcre:regex-replace-all "(OpenGL|hDC|hGLRC)"
                              name
                              (lambda (m r1)
                                (declare (ignore m))
                                (string-capitalize r1))
                              :simple-calls t))

(defun fix-api-names2 (name)
  (setf name (cl-ppcre:regex-replace-all "--" name "-"))
  (cl-ppcre:regex-replace-all "(Win-32)" name
                              (lambda (m r1)
                                (declare (ignore m))
                                (remove #\- r1))
                              :simple-calls t))

(defun add-dashes (name)
  (fix-api-names2
   (cl-ppcre:regex-replace-all "([a-z])([A-Z0-9])"
                               (fix-api-names (suffix-dashes name))
                               "\\1-\\2")))

#++
(mapcar 'add-dashes '( "XrGraphicsBindingOpenGLWin32KHR"
                      "XrGraphicsBindingOpenGLESAndroidKHR"
                      "XrGraphicsBindingD3D11KHR"
                      "xrGetD3D11GraphicsRequirementsKHR"
                      "boundingBox2DOutput"
                      "xrConvertTimeToWin32PerformanceCounterKHR"))
#++
("Xr-Graphics-Binding-Opengl-Win32-KHR"
 "Xr-Graphics-Binding-Opengl-Es-Android-KHR"
 "Xr-Graphics-Binding-D3D11-KHR"
 "xr-Get-D3D11-Graphics-Requirements-KHR"
 "bounding-Box-2D-Output"
 "xr-Convert-Time-To-Win32-Performance-Counter-KHR")


(defun %translate-type-name (name)
  (unless name
    (return-from %translate-type-name name))
  (let ((os (gethash name *os-types*))
        (platform (gethash name *platform-types*))
        (old (gethash name *old-names*)))
    (cond
      (os (second os))
      (platform platform)
      (old old)
      ((string= name "API Constants")
       'api-constants)
      ((string= name "enum")
       :enum)
      ((string= name "bitmask")
       :bitmask)
      (t
       (cond
         ((alexandria:starts-with-subseq "Xr" name)
          (intern (string-upcase (add-dashes (subseq name 2)))))
         ((alexandria:starts-with-subseq "XR_" name)
          (intern (substitute #\- #\_ (subseq name 3))))
         ((alexandria:starts-with-subseq "xr" name)
          (intern (string-upcase (add-dashes (subseq name 2)))))
         ((alexandria:starts-with-subseq "PFN_xr" name)
          (list :pointer (%translate-type-name (subseq name 4))))
         (t (error "todo ~s" name)))))))

(defun numeric-value (str)
  ;; ignore LL and u suffixes
  (setf str (string-right-trim "Lu" str))
  (cond
    ((not str) nil)
    ((alexandria:starts-with-subseq "0x" str)
     (parse-integer str :start 2 :radix 16))
    ((ignore-errors (parse-integer str)))
    ((and (alexandria:ends-with #\f str)
          (ignore-errors (parse-number:parse-number str :end (1- (length str))))))
    #++((multiple-value-bind (m matches)
            (ppcre:scan-to-strings "\\(~0U(LL)?(-1)?\\)" str)
          (when m
            ;; fixme: is this right on all platforms? (or any for that matter?)
            (let ((off (if (aref matches 1) -2 -1)))
              (if (aref matches 0)
                  (ldb (byte 64 0) off)
                  #-64-bit
                  (ldb (byte 32 0) off)
                  #+64-bit
                  (ldb (byte 64 0) off))))))
    (t
     (error "~s" str))))

#++
(defun extract-array-size (str)
  ;; things like <member><type>char</type>
  ;; <name>layerName</name>[<enum>XR_MAX_API_LAYER_NAME_SIZE</enum>]</member>,
  ;; probably should parse the <enum> etc at higher level, but just
  ;; trying to extract from string for now
  #++(when (position #\[ str)
       (assert (= 1 (count #\[ str)))
       (let* ((enum (subseq str (1+ (position #\[ str)) (position #\] str)))
              (count (if (get-constant enum)
                         (numeric-value (get-constant enum))
                         (numeric-value enum))))
         (assert (and enum count))
         (values enum count))))

(defun translate-var-name (name)
  (when (or (alexandria:starts-with-subseq "Xr" name)
            (alexandria:starts-with-subseq "XR_" name))
    (break "odd var name ~s?" name))
  (intern (string-upcase (add-dashes name))))

(defun make-keyword (name &key prefix)
  (if prefix
      (alexandria:format-symbol :keyword "~a/~a"
                                (string-upcase prefix)
                                (string-upcase name))
      (alexandria:make-keyword (string-upcase name))))

(defun make-const-symbol (name &key add-++)
  (let ((start (if (alexandria:starts-with-subseq "XR_" name) 3 0)))
    (intern (if add-++
                (format nil "+~a+"
                        (string-upcase
                         (subseq (substitute #\- #\_ name) start)))
                (string-upcase (subseq (substitute #\- #\_ name) start)))
            '#:3b-openxr-parse-spec2)))

(defun make-const-keyword (name)
  (let ((start (if (alexandria:starts-with-subseq "XR_" name) 3 0)))
    (alexandria:make-keyword
     (subseq (substitute #\- #\_ name) start))))

(defun make-enum-name (name parent)
  ;; fixme: do this with directly strings instead of making symbols
  (let ((parent (%translate-type-name parent))
        (name (make-const-keyword name)))
    (let ((a (mismatch (string name) (string parent))))
      (make-keyword
       (string-trim
        "-"
        (cl-ppcre:regex-replace-all
         "-BIT$"
         (cl-ppcre:regex-replace-all "-BIT-" (subseq (string name) a) "-")
         ""))))))
