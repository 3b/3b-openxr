(asdf:load-systems 'alexandria 'cl-ppcre 'split-sequence 'cxml 'xpath 'cxml-stp)

(defvar *package-name* "3b-openxr")
(defvar *bindings-package-name* "3b-openxr-bindings")
(defvar *core-definer* 'defcfun)
(defvar *ext-definer* 'defextfun)
(defvar *noprint* nil)
(defvar *override-function-name* nil)

(defvar *ext-enum-base* (floor 1e10))
(defvar *ext-enum-block* 1000)
(defvar *api-version* nil)
(defvar *api-last-updated* nil)
(defvar *exports* (make-hash-table :test 'equalp))
(defvar *enum-values* (make-hash-table :test 'equalp))
(defvar *printed-structs* (make-hash-table))
(defvar *constants* (make-hash-table :test 'equal))

(defparameter *defines*
  ;; some manually extracted #defines, not sure if these should be
  ;; +foo+ style or :foo style? need to manually adjust applicable arg
  ;; types for latter, so ++ for now
  '(+null-path+ 0
    +null-system-id+ 0
    +no-duration+ 0
    +infinite-duration+ #x7fffffffffffffff
    +min-haptic-duration+ -1
    +frequency-unspecified+ 0
    +hand-joint-count-ext+ 26))

(defparameter *define-types*
  (alexandria:plist-hash-table '() :test 'equal))

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
     )
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
     "PFN_xrDebugUtilsMessengerCallbackEXT" :pointer
     "PFN_xrVoidFunction" :pointer
     )
   :test 'equalp))

(defparameter *bitmask-types* (make-hash-table))
(defparameter *enum-types* (make-hash-table))
(defparameter *struct-types* (make-hash-table))
(defparameter *struct-dependencies* (make-hash-table))

(defun format-comment (s &key prefix)
  (with-input-from-string (ss s)
    (loop for l = (read-line ss nil nil)
          while l do (format t "~@[~a~];; ~a~%" prefix l))))

(defun xps (node)
  (let ((s (string-trim '(#\space #\tab #\newline) (xpath:string-value node))))
    (unless (string= s "") s)))

(defun numeric-value (str)
  (cond
    ((not str) nil)
    ((alexandria:starts-with-subseq "0x" str)
     (parse-integer str :start 2 :radix 16))
    ((ignore-errors (parse-integer str)))
    ((and (alexandria:ends-with #\f str)
          (ignore-errors (parse-number:parse-number str :end (1- (length str))))))
    ((multiple-value-bind (m matches)
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

(defun suffix-dashes (name)
  (cl-ppcre:regex-replace-all "(.)([1-4]?[if]|KHR|ARM|COLLABORA|EPIC|EXT|GOOGLE|INTEL|LUNARG|MND|MSFT|NV|OCULUS|PLUTO|QCOM|STARBREEZE|VALVE|VARJO)$"
                              name "\\1-\\2"))
(defun fix-api-names (name)
  (cl-ppcre:regex-replace-all "(OpenGL|hDC|hGLRC)" name
                              (lambda (m r1)
                                (declare (ignore m))
                                (string-capitalize r1))
                              :simple-calls t))

(defun add-dashes (name)
  (cl-ppcre:regex-replace-all "([a-z])([A-Z0-9])"
                              (fix-api-names (suffix-dashes name))
                              "\\1-\\2"))

(defun make-enum-name (name parent)
  ;; fixme: do this with directly strings instead of making symbols
  (let ((parent (translate-type-name parent))
        (name (make-const-keyword name)))
    (let ((a (mismatch (string name) (string parent))))
      (make-keyword
       (string-trim
        "-"
        (cl-ppcre:regex-replace-all
         "-BIT$"
         (cl-ppcre:regex-replace-all "-BIT-" (subseq (string name) a) "-")
         ""))))))

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
          (list :pointer (translate-type-name (subseq name 4))))
         (t (error "todo ~s" name)))))))

(defun translate-type-name (name &key len str)
  (let ((type (%translate-type-name name))
        (pointer (position #\* str))
        (array (position #\[ str)))
    (when (and (eql type :char) (search "null-terminated" len))
      (setf type :string))
    (when (gethash type *struct-types*)
      (setf type (list :struct type)))
    (when (and pointer array)
      (error "todo: pointer + array str"))
    (when pointer
      (loop repeat (count #\* str)
            do (setf type (list :pointer type))))
    (when array
      ;; things like <member><type>char</type>
      ;; <name>layerName</name>[<enum>XR_MAX_API_LAYER_NAME_SIZE</enum>]</member>,
      ;; probably should parse the <enum> etc at higher level, but
      ;; just trying to extract from string for now
      (assert (= 1 (count #\[ str)))
      (let ((count (subseq str
                           (1+ (position #\[ str))
                           (position #\] str))))
        (assert count)
        (setf count (if (get-constant count)
                        (numeric-value (get-constant count))
                        (numeric-value count)))
        ;; :@ is hack to mark lists that should be expanded in caller
        (setf type (list :@ type :count count))))
    type))

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

(defun make-const-keyword (name)
  (let ((start (if (alexandria:starts-with-subseq "XR_" name) 3 0)))
    (alexandria:make-keyword
     (subseq (substitute #\- #\_ name) start))))

(defun add-constant (name value)
  (assert (or (not (nth-value 1 (gethash name *constants*)))
              (equalp value (gethash name *constants*))))
  (setf (gethash name *constants*) value))

(defun get-constant (name)
  (gethash name *constants*))

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


(defvar *spec-dir* (asdf:system-relative-pathname '3b-openxr "spec/"))

(defvar *xml* (cxml:parse-file
               (merge-pathnames "xr.xml" *spec-dir*)
               (cxml:make-whitespace-normalizer
                (stp:make-builder))))

(defvar *vendor-ids* (make-hash-table :test 'equal))
(defvar *tags* (make-hash-table :test 'equal))

(defun attrib-plist (node)
  (loop for a in (stp:list-attributes node)
        for v = (stp:attribute-value node (stp:local-name a))
        collect (make-keyword (stp:local-name a))
        collect v))


(defmethod node (name node)
  (break "node ~s not handled yet" name))

(defmethod node ((name (eql :comment)) node)
  (format-comment (xps node))
  (setf *noprint* t))

(defmacro defnode (tag (&rest keys) &body body)
  `(defmethod node ((name (eql ,tag)) node)
     (let ((%attribs (attrib-plist node))
           (*noprint* *noprint*))
       (destructuring-bind (&key ,@keys) %attribs
         ,@body
         (stp:do-children (x node)
           (typecase x
             (stp:text
              (when (xps x)
                (unless *noprint* (format t " text ~s~%"  (xps x)))))
             (stp:comment
              (when (xps x)
                (unless *noprint* (format-comment (xps x)))))
             (t (node (make-keyword (stp:local-name x) :prefix ,tag) x))))))))

(defnode :vendorids/vendorid (name comment id)
  (declare (ignorable comment id))
  (setf (gethash name *vendor-ids*) %attribs))


(defnode :vendorids ())

(defnode :tags/tag (name contact author)
  (declare (ignore contact author))
  (setf (gethash name *tags*) %attribs))

(defnode :tags ())

(defvar *current-type* nil)
(defnode :types/type/name ())
(defnode :types/type/type ())
(defnode :types/type/member (values optional len noautovalidity)
  (when *current-type*
    (let ((name (translate-var-name (xps (xpath:evaluate "name" node))))
          (type (translate-type-name (xps (xpath:evaluate "type" node))
                                     :len len :str (xps node)))
          (extra nil))
      (when len
        (setf len
              (mapcar
               'translate-var-name
               (remove "null-terminated"
                       (split-sequence:split-sequence #\, len)
                       :test 'equal)))
        (ecase (length len)
          (0 (setf len nil))
          (1 (setf len (car len)))))
      (when values
        (setf (gethash name
                       (getf (gethash (car *current-type*) *struct-types*) :init))
              values)
        (setf extra (format nil "= ~(~s~)" (translate-type-name values))))
      (when (and noautovalidity (not (string= "" noautovalidity)))
        (format *debug-io* "ct ~s~%" *current-type*)
        (unless *current-type*
          (format *debug-io*
                  "node = ~s~%" (attrib-plist (stp:parent node)))
          (format *debug-io*
                  "node = ~s~%" (xpath:string-value (stp:parent node))))
        (setf (gethash name
                       (getf (gethash (car *current-type*) *struct-types*)
                             :no-auto-validity))
              (if (string-equal "true" noautovalidity)
                  t
                  (make-keyword noautovalidity)))
        (setf extra (format nil "~@[~a ~]noautovalidity" extra)))
      (when optional
        (pushnew name
                 (getf (gethash (car *current-type*) *struct-types*) :optional))
        (setf extra (format nil "~@[~a ~]optional" extra)))
      (cond
        (len
         (assert (plusp (count #\* (xps node))))
         (loop for i below (count #\* (xps node))
               do (setf type (list :pointer type)))
         (setf (gethash name
                        (getf (gethash (car *current-type*) *struct-types*)
                              :counted-slots))
               len)
         (format t "~&  ~((~a ~s)~) ;; count ~(~s~@[, ~a~]~)~%"
                 name type  len extra))
        (t
         (loop for i below (count #\* (xps node))
               do (setf type (list :pointer type)))
         (if (typep type '(cons (eql :@)))
             (format t "~&  ~((~a~{ ~s~})~)~@[ ;; ~a~%~]" name (cdr type) extra)
             (format t "~&  ~((~a ~s)~)~@[ ;; ~a~%~]" name type extra))))))
  (setf *noprint* t))

(defnode :types/type/member/type ())
(defnode :types/type/member/name ())
(defnode :types/type/member/enum ())

(defun get-one-node (xpath xml)
  (let ((ns (xpath:evaluate xpath xml)))
    (unless (and (xpath:node-set-p ns)
                 (not (xpath:node-set-empty-p ns)))
      (break "couldn't find node?~% q= ~s~%" xpath))
    (let* ((nls (xpath:all-nodes ns)))
      (when (cdr nls)
        (break "found multiple nodes?~%q=~s~%" xpath))
      (car nls))))

(defun print-struct-deps (name)
  (let* ((deps (gethash name *struct-dependencies*))
         (unprinted (loop for i in deps unless (gethash i *printed-structs*)
                          collect i)))
    (when unprinted
      (format *debug-io* ";;; print ~s early...~%" unprinted)
      (loop for dep in unprinted
            for orig = (gethash dep *struct-types*)
            do (assert (stringp orig))
               (let ((n (xpath:with-variables (("name" orig))
                          (get-one-node "/registry/types/type[@name=$name]"
                                        *xml*))))
                 (format *debug-io* "--> ~s~%" (xpath:string-value n))
                 (assert n)
                 (node :types/type n))))))

(defnode :types/type (category name requires bitvalues parent mayalias
                      returnedonly protect structextends parentstruct
                      alias)
  (declare (ignorable category name requires bitvalues parent mayalias
                      returnedonly protect structextends parentstruct
                      alias))
  ;; types from OS headers or openxr_platform_defines.h\
  (when (and requires (not category))
    (unless (or (gethash name *os-types*)
                (gethash name *platform-types*))
      (break "unknown type ~s (~a) ;; ~s" name (copy-seq name) requires))
    (return-from node nil))
  (when category
    (ecase (make-keyword category)
      (:include
       (let ((name (make-const-keyword (string-upcase name)))
             (str (xps node)))
         (assert (string= str (gethash name *defines-xmlval*)))
         (setf *noprint* t)))
      (:define
       (let ((name (make-const-keyword (xps (xpath:evaluate "name" node))))
             (str (xps node))
             (ver (xps (xpath:evaluate "type/following-sibling::text()[1]" node))))
         ;; extract api version
         (when (eql name :current-api-version)
           (setf *api-version*
                 (mapcar 'parse-integer
                         (split-sequence:split-sequence
                          #\, (string-trim "()" ver)))))
         ;; make sure #defines haven't been added or changes
         #++(assert (string= str (gethash name *defines-xmlval*)))
         (unless (string= str (gethash name *defines-xmlval*))
           (let ((n (second (print (split-sequence:split-sequence #\space str)))))
             (break "unknown define ~s~%~s ~s~%"
                    str (make-const-keyword n) (copy-seq str))))
         (setf *noprint* t)))
      (:basetype
       (let ((name (translate-type-name (xps (xpath:evaluate "name" node))))
             (type (translate-type-name (xps (xpath:evaluate "type" node)))))
         (format t "~&~((defctype ~s ~s)~)~%~%" name type)
         (return-from node nil)))
      (:bitmask
       (let* ((tname (translate-type-name (xps (xpath:evaluate "name" node))))
              (bv (translate-type-name bitvalues))
              (definition (or (gethash tname *bitmask-types*)
                              (gethash bv *bitmask-types*))))
         (assert definition)
         (destructuring-bind (&key name comment values def type)
             definition
           (declare (ignorable name))
           (when comment
             (format-comment comment))
           (if type
               (format t "~&~((~a (~s ~s)~)" def tname type)
               (format t "~&~((~a ~a~)" def tname))
           (loop for v in (reverse values)
                 do (format t "~&~(~a~)" v))
           (format t ")~%~%"))
         (setf *noprint* t)))
      (:handle
       (let ((name (translate-type-name (xps (xpath:evaluate "name" node))))
             (type (translate-type-name (xps (xpath:evaluate "type" node)))))
         (format t "~&~((defctype ~s ~s)~)~%" name type)
         (setf *noprint* t)))
      (:enum
       (let* ((name (translate-type-name name))
              (definition (gethash name *enum-types*)))
         (when definition
           (destructuring-bind (&key name comment values def type)
               definition
             (when comment
               (format-comment comment))
             (if type
                 (format t "~&~((~a (~s ~s)~)" def name type)
                 (format t "~&~((~a ~a~)" def name))
             (loop for v in (reverse values)
                   do (format t "~&~(~a~)" v))
             (format t ")~%~%")))
         (setf *noprint* t)))
      (:struct
       (let* ((name (%translate-type-name name))
              (plist (list :name name :init (make-hash-table)
                           :counted-slots (make-hash-table)
                           :no-auto-validity (make-hash-table))))
         (when parent
           (setf (getf plist :parent) parent))
         (when requires
           (setf (getf plist :requires) requires))
         (when bitvalues
           (setf (getf plist :bitvalues) bitvalues))
         (unless (gethash name *printed-structs*)
           (setf (gethash name *printed-structs*) t)
           (print-struct-deps name)
           (format *debug-io* "ct ~s~% -> ~s~%" *current-type* name)
           (push name *current-type*)
           (setf (gethash name *struct-types*) plist)
           (setf (gethash name *exports*) t)
           (format t "~&(defcstruct ~(~a~)" name))))
      (:funcpointer
       (format t "~@<;;;~@; ~a~%~:>~%" (xps node))
       (setf *noprint* t))))
  (when (not category)
    (format t "type: ~s ~s~%" name category)))

(defmethod node :after ((name (eql :types/type)) node)
  (when (and (string-equal "struct" (getf (attrib-plist node) :category))
             *current-type*)
    (format *debug-io* "ct <- ~s~%" *current-type*)
    (pop *current-type*)
    (format t ")~%~%")))

(defnode :types/comment ()
  (format-comment (xps node)))
(defnode :types ()
  (format t ";; types :~%"))

(defnode :enums/enum (name value comment bitpos)
  (declare (ignorable name value comment bitpos)))


(defnode :enums/unused (start)
  (declare (ignore start)))

(defnode :enums (name type comment)
  (declare (ignore name type comment)))

(defnode :commands/command/proto/type ())
(defnode :commands/command/proto/name ())

(defnode :commands/command/proto ()
  (destructuring-bind () (attrib-plist node)
    (let ((type (xps (xpath:evaluate "type" node)))
          (name (or *override-function-name*
                    (xps (xpath:evaluate "name" node)))))
      (unless type
        (break ",kjhg"))
      (setf (gethash (translate-type-name name) *exports*) t)
      (format t "~&(~(~s~) (~s ~(~s~)) ~(~s~)"
              *core-definer*
              name
              (translate-type-name name)
              (translate-type-name type))))
  (setf *noprint* t))

(defnode :commands/command/param/type ())
(defnode :commands/command/param/name ())
(defnode :commands/command/param/enum ())

(defnode :commands/command/param (optional len externsync)
  (let ((type (translate-type-name (xps (xpath:evaluate "type" node))
                                   :len len :str (xps node)))
        (name (translate-var-name (xps (xpath:evaluate "name" node)))))
    (when len
      (format t "~&  ;; count = ~(~s~)~%" (translate-var-name len)))
    (when externsync
      (format t "~&  ;; externsync = ~(~a~)~%" externsync))
    (when optional
      (format t "~&  ;; optional = ~(~a~)~%" optional))
    (format t "~& ~((~s ~s)~)" name type)
    (setf *noprint* t)
    #++(format t "  arg: ~s -> ~s ~%" name type)))

(defnode :commands/command/implicitexternsyncparams/param ())
(defnode :commands/command/implicitexternsyncparams ()
  (format t "~&  ;; implicit external sync params:~%")
  (format-comment (xps node) :prefix "  ")
  (setf *noprint* t))

(defmethod node :around ((tag (eql :commands/command)) node)
  (let* ((%attribs (attrib-plist node))
         (alias (getf %attribs :alias)))
    (cond
      (alias
       (xpath:with-variables (("name" alias))
         (let* ((name (getf %attribs :name))
                (fun (get-one-node "/registry/commands/command[proto/name=$name]" *xml*)))
           (format *debug-io* ";; alias ~s -> ~s~%" name alias)
           (format t ";; alias ~s -> ~s~%" name alias)
           (let ((*override-function-name* name))
             (node :commands/command fun)))))
      (t (call-next-method)))))

(defnode :commands/command (errorcodes successcodes name alias)
  (declare (ignore name alias))
  (format t ";; success ~(~a~)~%" (make-const-keyword successcodes))
  (format-comment
   (format nil " errors ~(~a~)~%" (mapcar 'make-const-keyword
                                          (split-sequence:split-sequence
                                           #\, errorcodes)))))

(defmethod node :after ((tag (eql :commands/command)) node)
  (format t ")~%~%"))

(defnode :commands ())


(defnode :feature/require/type (name)
  (declare (ignore name)))
(defnode :feature/require/enum (name)
  (declare (ignore name)))
(defnode :feature/require/command (name)
  (declare (ignore name)))
(defnode :feature/require (comment)
  (declare (ignore comment)))
(defnode :feature (api name number)
  (declare (ignore api name number)))

(defnode :extensions ())
(defnode :extensions/extension (protect supported type number name requires
                                provisional promotedto)
  (declare (ignore protect supported type number name requires provisional
                   provisional promotedto)))
(defnode :extensions/extension/require (extension)
  (declare (ignore extension)))
(defnode :extensions/extension/require/enum (name value comment dir offset extends alias bitpos)
  (declare (ignore name value comment dir offset extends alias bitpos)))
(defnode :extensions/extension/require/type (name)
  (declare (ignore name)))
(defnode :extensions/extension/require/command (name)
  (declare (ignore name)))
(defnode :extensions/extension/require/extend (interaction_profile_path)
  (declare (ignore interaction_profile_path)))
(defnode :extensions/extension/require/extend/component (type subpath)
  (declare (ignore type subpath)))

(defnode :interaction_profiles ()
  )

(defnode :feature/require/interaction_profile (name)
  (declare (ignore name)))

(defnode :interaction_profiles/interaction_profile (name title)
  (declare (ignore name title)))

(defnode :interaction_profiles/interaction_profile/user_path (path)
  (declare (ignore path)))

(defnode :interaction_profiles/interaction_profile/component (type subpath
                                                              system user_path)
  (declare (ignore type subpath system user_path)))

(defnode :extensions/extension/require/interaction_profile (name)
  (declare (ignore name)))

(defun collect-enum-values (xml)
  (clrhash *enum-types*)
  (clrhash *bitmask-types*)
  (clrhash *enum-values*)
  (clrhash *constants*)
  ;; add base enum/bitmask types
  (xpath:do-node-set (node (xpath:evaluate "/registry/enums" xml))
    (destructuring-bind (&key name type comment)
        (attrib-plist node)
      (let ((parent name)
            children)
        (xpath:do-node-set (child (xpath:evaluate "enum" node))
          (destructuring-bind (&key name value comment bitpos)
              (attrib-plist child)
            (when bitpos
              (assert (not value))
              (setf value (format nil "#x~8,'0x"
                                  (expt 2 (parse-integer bitpos)))))
            (setf (gethash (list parent name) *enum-values*)
                  value)
            (add-constant name value)
            (push
             (with-output-to-string (*standard-output*)
               (when comment
                 (format-comment comment :prefix "  "))
               (format t "  ~((~s ~a)~)" (make-enum-name name parent) value))
             children)))
        (setf (gethash (translate-type-name name) *exports*) t)
        (cond
          ((or (not type) (string= type "enum"))
           (setf (gethash (translate-type-name name) *enum-types*)
                 (list :name (translate-type-name name)
                       :def 'defcenum
                       :comment comment
                       :values children)))
          ((string= type "bitmask")
           (setf (gethash (translate-type-name name) *bitmask-types*)
                 (list :name (translate-type-name name)
                       :def 'defbitfield
                       :comment comment
                       :values children)))
          (t (error "unknown enum type ~s?" type))))))
  ;; add any enums from extensions
  (xpath:do-node-set (ext (xpath:evaluate "/registry/extensions/extension"
                                          xml))
    (destructuring-bind (&key protect supported type number name requires
                           provisional promotedto)
        (attrib-plist ext)
      (declare (ignorable protect supported type number name requires
                          provisional promotedto))
      #++(format t "~%ext ~s: ~s ~s ~s ~s ~s~%"
                 name number type requires protect supported)
      (let (ext-name ext-version)
        (xpath:do-node-set (enum (xpath:evaluate "require/enum" ext))
          (destructuring-bind (&key name value comment dir extends offset
                                 alias bitpos)
              (attrib-plist enum)
            (cond
              ((alexandria:ends-with-subseq "_SPEC_VERSION" name)
               (setf ext-version (parse-integer value)))
              ((alexandria:ends-with-subseq "_EXTENSION_NAME" name)
               (setf ext-name value))
              ((and extends offset)
               ;; fixme: handle bitfields in extensions
               (when (search "BIT" name)
                 (break "~a" name))
               (let ((v (ext-enum-value number dir offset)))
                 (add-constant name v)
                 (setf (gethash (list extends name) *enum-values*) v)
                 (push
                  (format nil "~@[  ;; ~a~%~]  ~((~s ~a)~)"
                          comment
                          (make-enum-name name extends) v)
                  (getf (gethash (translate-type-name extends) *enum-types*)
                        :values))))
              ;; try to detect untyped enums
              ((and name value (not (or dir extends offset)))
               (add-constant name value)
               (setf (gethash (list extends name) *enum-values*) value)
               (push
                (format nil "~@[  ;; ~a~%~]  ~((~s ~a)~)"
                        comment
                        (make-enum-name name nil) value)
                (getf (gethash 'api-constants *enum-types*) :values)))
              (alias
               (let* ((v (gethash (list extends alias) *enum-values*))
                      (tx (translate-type-name extends))
                      (base (or (gethash tx *enum-types*)
                                (gethash tx *bitmask-types*))))
                 (unless v
                   (break "couldn't find enum~% ~s ::~% ~s~% extends alias~% from ~s"
                          extends alias name))
                 (format *debug-io* "enum alias ~s:: ~s -> ~s (~s)~%"
                         extends name alias v)
                 (push
                  (format nil "  ;; alias ~a~%  ;;    -> ~a~%~@[  ;; ~a~%~]  ~((~s ~a)~)"
                          name alias
                          comment
                          (make-enum-name name extends) v)
                  (getf base :values))))
              (bitpos
               (assert extends)
               (assert (not value))
               (setf value (format nil "#x~8,'0x"
                                   (expt 2 (parse-integer bitpos))))
               (add-constant name value)
               (setf (gethash (list extends name) *enum-values*) value)
               (push
                (format nil "~@[  ;; ~a~%~]  ~((~s ~a)~)"
                        comment
                        (make-enum-name name extends) value)
                (getf (gethash (translate-type-name extends) *bitmask-types*)
                      :values)))
              (t (break "?")))))))))

(defun collect-struct-names (xml)
  (clrhash *struct-types*)
  (clrhash *struct-dependencies*)
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category='struct']" xml))
    (let* ((name (getf (attrib-plist node) :name))
           (tname (%translate-type-name name)))
      (format t "struct ~s ~s~%" name tname)
      (setf (gethash tname *struct-types*) name)
      (xpath:do-node-set (type-node (xpath:evaluate "member/type" node))
        (let ((type (xpath:string-value type-node)))
          #++(format t "~s~%" type)
          (pushnew type (gethash tname *struct-dependencies*) :test 'string=)))))
  (maphash (lambda (k v)
             (let ((v (remove nil (mapcar (lambda (a)
                                            (and (gethash (%translate-type-name a)
                                                          *struct-types*)
                                                 (%translate-type-name a)))
                                          v))))
               (if v
                   (setf (gethash k *struct-dependencies*) v)
                   (remhash k *struct-dependencies*))))
           *struct-dependencies*))

#++
(progn
  (collect-enum-values *xml*)
  (xpath:do-node-set (x (xpath:evaluate "/registry/*" *xml*))
    (node (make-keyword (stp:local-name x)) x)))

(progn
  (collect-enum-values *xml*)
  (collect-struct-names *xml*)
  (clrhash *exports*)
  (clrhash *printed-structs*)
  (alexandria:with-output-to-file (*standard-output*
                                   (asdf:system-relative-pathname
                                    :3b-openxr "bindings.lisp")
                                   :if-exists :supersede)
    (format t "(in-package #:~a)~%"
            *bindings-package-name*)
    (xpath:do-node-set (x (xpath:evaluate "/registry/*" *xml*))
      (node (make-keyword (stp:local-name x)) x)))
  (alexandria:with-output-to-file (*standard-output*
                                   (asdf:system-relative-pathname
                                    :3b-openxr "bindings-package.lisp")
                                   :if-exists :supersede)
    (format t "(defpackage #:~a~%" *bindings-package-name*)
    (format t "  (:use :cl #:cffi)~%")
    (format t "  (:shadow #:space #:time #:atom)~%")
    (format t "  (:export ~(~{#:~s~^~%           ~}~)))~%"
            (sort (alexandria:hash-table-keys *exports*)
                  'string< :key 'string-downcase))))
