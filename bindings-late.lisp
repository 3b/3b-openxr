(in-package #:3b-openxr-bindings)

;; code to finish setting up extension handling after bindings are
;; generated


;; NULL instance was defined before *function-getters* was initialized,
;; so fill it with correct functions
(setf (%instance-pointers *instance*) (copy-seq *function-getters*))

