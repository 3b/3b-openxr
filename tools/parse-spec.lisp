(asdf:load-systems 'alexandria 'cl-ppcre 'split-sequence 'cxml 'xpath 'cxml-stp)

(defvar *package-name* "3b-openxr")
(defvar *bindings-package-name* "3b-openxr-bindings")
(defvar *core-definer* 'defcfun)
(defvar *ext-definer* 'defextfun)
(defvar *noprint* nil)

(defvar *ext-enum-base* (floor 1e10))
(defvar *ext-enum-block* 1000)
(defvar *api-version* nil)
(defvar *api-last-updated* nil)
(defvar *exports* (make-hash-table :test 'equalp))

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
     "PFNEGLGETPROCADDRESSPROC" ((:pointer :void) pfn-egl-get-proc-address-proc) ;; "EGL/egl.h"
     "GLXFBConfig" (:opaque glx-fb-config)       ;;"GL/glxext.h"
     "GLXDrawable" (xid glx-drawable)            ;;"GL/glxext.h"
     "GLXContext" (xid glx-context)              ;;"GL/glxext.h"
     "HGLRC" (handle hglrc)                      ;;"GL/wglext.h"
     "wl_display" (:opaque wl-display)           ;;"wayland-client.h"
     "HDC" (handle hdc)                          ;;"windows.h"
     "LUID" (:opaque luid)                       ;;"windows.h"
     "LARGE_INTEGER" (:int64 large-integer)      ;;"windows.h"
     "Display" (:opaque display)                  ;;"X11/Xlib.h"
     "VisualID" (:unsigned-long visual-id)       ;;"X11/Xlib.h"
     "Window" (xid window)                       ;;"X11/Xlib.h"
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
     "VkPhysicalDevice" (:opaque vk-physical-device) ;;"vulkan/vulkan.h"
     "timespec" ((:struct timespec) timespec)        ;;"time.h"
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
  (let ((type (%translate-type-name name)))
    (when (and (eql type :char) (search "null-terminated" len))
      (setf type :string))
    (when (gethash type *struct-types*)
      (setf type (list :struct type)))
    (when (position #\* str)
      (loop repeat (count #\* str)
            do (setf type (list :pointer type))))
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

(defvar *current-type*)
(defnode :types/type/name ())
(defnode :types/type/type ())
(defnode :types/type/member (values optional len noautovalidity)
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
                     (getf (gethash *current-type* *struct-types*) :init))
            values)
      (setf extra (format nil "= ~(~s~)" (translate-type-name values))))
    (when (and noautovalidity (not (string= "" noautovalidity)))
      (setf (gethash name
                     (getf (gethash *current-type* *struct-types*)
                           :no-auto-validity))
            (if (string-equal "true" noautovalidity)
                t
                (make-keyword noautovalidity)))
      (setf extra (format nil "~@[~a ~]noautovalidity" extra)))
    (when optional
      (pushnew name
               (getf (gethash *current-type* *struct-types*) :optional))
      (setf extra (format nil "~@[~a ~]optional" extra)))
    (cond
      (len
       (assert (plusp (count #\* (xps node))))
       (loop for i below (count #\* (xps node))
             do (setf type (list :pointer type)))
       (setf (gethash name
                      (getf (gethash *current-type* *struct-types*)
                            :counted-slots))
             len)
       (format t "~&  ~((~a ~s)~) ;; count ~(~s~@[, ~a~]~)~%"
               name type  len extra))
      (t
       (loop for i below (count #\* (xps node))
             do (setf type (list :pointer type)))
       (format t "~&  ~((~a ~s)~)~@[ ;; ~a~%~]" name type extra))))
  (setf *noprint* t))

(defnode :types/type/member/type ())
(defnode :types/type/member/name ())
(defnode :types/type/member/enum ())
(defnode :types/type (category name requires bitvalues parent mayalias
                      returnedonly protect structextends parentstruct)
  (declare (ignorable category name requires bitvalues parent mayalias
                      returnedonly protect structextends parentstruct))
  ;; types from OS headers or openxr_platform_defines.h\
  (when (and requires (not category))
    (unless (or (gethash name *os-types*)
                (gethash name *platform-types*))
      (break "unknown type ~s (~s)" name requires))
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
         (assert (string= str (gethash name *defines-xmlval*)))
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
         (setf *current-type* name)
         (setf (gethash name *struct-types*) plist)
         (setf (gethash name *exports*) t)
         (format t "~&(defcstruct ~(~a~)" name)))
      (:funcpointer
       (format t "~@<;;;~@; ~a~%~:>~%" (xps node))
       (setf *noprint* t))))
  (when (not category)
    (format t "type: ~s ~s~%" name category)))

(defmethod node :after ((name (eql :types/type)) node)
  (when (string-equal "struct" (getf (attrib-plist node) :category))
    (setf *current-type* nil)
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
          (name (xps (xpath:evaluate "name" node))))
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

(defnode :commands/command (errorcodes successcodes)
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
                                provisional)
  (declare (ignore protect supported type number name requires provisional)))
(defnode :extensions/extension/require ())
(defnode :extensions/extension/require/enum (name value comment dir offset extends)
  (declare (ignore name value comment dir offset extends)))
(defnode :extensions/extension/require/type (name)
  (declare (ignore name)))
(defnode :extensions/extension/require/command (name)
  (declare (ignore name)))




(defun collect-enum-values (xml)
  (clrhash *enum-types*)
  (clrhash *bitmask-types*)
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
                           provisional)
        (attrib-plist ext)
      (declare (ignorable protect supported type number name requires
                          provisional))
      #++(format t "~%ext ~s: ~s ~s ~s ~s ~s~%"
                 name number type requires protect supported)
      (let (ext-name ext-version)
        (xpath:do-node-set (enum (xpath:evaluate "require/enum" ext))
          (destructuring-bind (&key name value comment dir extends offset)
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
                 (push
                  (format nil "~@[  ;; ~a~%~]  ~((~s ~a)~)"
                          comment
                          (make-enum-name name extends) v)
                  (getf (gethash (translate-type-name extends) *enum-types*)
                        :values))))
              (t (break "?")))))))))

(defun collect-struct-names (xml)
  (clrhash *struct-types*)
  (xpath:do-node-set (node (xpath:evaluate "/registry/types/type[@category='struct']" xml))
    (format t "struct ~s ~s~%"
            (getf (attrib-plist node) :name)
            (%translate-type-name (getf (attrib-plist node) :name))
            )
    (setf (gethash (%translate-type-name (getf (attrib-plist node) :name))
                   *struct-types*)
          t)))

#++
(progn
  (collect-enum-values *xml*)
  (xpath:do-node-set (x (xpath:evaluate "/registry/*" *xml*))
    (node (make-keyword (stp:local-name x)) x)))

(progn
  (collect-enum-values *xml*)
  (collect-struct-names *xml*)
  (clrhash *exports*)
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
