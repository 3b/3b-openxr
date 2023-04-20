(in-package #:3b-openxr-wrappers)

;;; random user-level helpers and utilities

(defun extension-available-p (name &key layer)
  (let ((n (enumerate-instance-extension-properties layer)))
    (loop for i in n thereis (string= name (getf i :name)))))

(defun layer-available-p (layer)
  (let ((n (enumerate-api-layer-properties)))
    (loop for i in n thereis (string= layer (getf i :name)))))



