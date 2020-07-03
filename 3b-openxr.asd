(defsystem 3b-openxr
  :description "Common Lisp bindings for the OpenXR API"
  :depends-on (#++ cffi-libffi cffi alexandria trivial-features)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "bindings-package")
               (:file "types")
               (:file "bindings")
               ;; todo: wrappers
               ))
