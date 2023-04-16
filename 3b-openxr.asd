
(defsystem 3b-openxr
  :description "Common Lisp bindings for the OpenXR API"
  :depends-on (cffi alexandria trivial-features 3b-openxr/bindings)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "package")
               (:file "mid-level")
               (:file "generated-macros")
               (:file "util")
               (:file "macros")
               (:file "wrappers-2")
               (:file "wrappers-3")
               (:file "wrappers-4")
               (:file "wrappers-5")
               (:file "wrappers-6")
               (:file "wrappers-7")
               (:file "wrappers-8")
               (:file "wrappers-9")
               (:file "wrappers-10")
               (:file "wrappers-11")
               (:file "wrappers-extensions")
               ))


(defsystem 3b-openxr/bindings
  :description "Common Lisp bindings for the OpenXR API (low-level part)"
  :depends-on (cffi alexandria trivial-features)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:file "bindings-package")
               (:file "library")
               (:file "bindings-utils")
               (:file "types")
               (:file "bindings")
               (:file "constants")
               (:file "manual-structs")
               (:file "bindings-late")
               ))
#++
(ql:quickload '3b-openxr)
