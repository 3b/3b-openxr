
(defsystem 3b-openxr-generator
  :description "Internal tools for generating bindings for 3b-openxr"
  :depends-on (cffi alexandria cl-ppcre split-sequence cxml xpath cxml-stp)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components ((:module "tools"
                :serial t
                :components ((:file "common")
                             (:file "defines")
                             (:file "parse-spec")
                             (:file "schema")
                             (:file "methods")
                             (:file "spec")
                             (:file "bindings")
                             ))))
