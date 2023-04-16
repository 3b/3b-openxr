(in-package #:3b-openxr-bindings)


(cffi:define-foreign-library openxr-loader
  (:windows (:or "libopenxr_loader.dll"
                 "openxr-loader.dll"))
  (:linux (:or "libopenxr_loader.so"
               "libopenxr_loader.so.1")))

(cffi:use-foreign-library openxr-loader)
