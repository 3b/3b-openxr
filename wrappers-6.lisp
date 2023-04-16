(in-package #:3b-openxr-wrappers)

;;; 6. Path Tree and Semantic Paths

;; 6.1. Path Atom Type
;; 6.2. Well-Formed Path Strings
(defun string-to-path (path)
  (with-returned-atom (p %:path)
    (%:string-to-path *instance* path p)))

(defun path-to-string (path)
  (with-two-call/string (i o p)
    (%:path-to-string *instance* path i o p)))

;; 6.3. Reserved Paths
;; 6.4. Interaction Profile Paths
