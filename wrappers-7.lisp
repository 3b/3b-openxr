(in-package #:3b-openxr-wrappers)

;;; 7. Spaces

;; 7.1. Reference Spaces

(defun get-reference-space-bounds-rect (session type)
  (with-extent-2d-f (e :%slots t)
    (let ((r (check-result (%:get-reference-space-bounds-rect session type e))))
      (if (eql r :space-bounds-unavailable)
          ;; todo: should this return 0,0 with 2nd value for success
          ;; instead?
          :space-bounds-unavailable
          (extent-2d-f %:width %:height)))))


;; 7.2. Action Spaces

;; 7.3. Space Lifecycle

(defun enumerate-reference-spaces (session)
  (with-two-call (i o p %:reference-space-type)
    (%:enumerate-reference-spaces session i o p)))

(defun create-reference-space (session reference-space-type
                               &key (pose '(%:position #(0 0 0)
                                            %:orientation #(0 0 0 1))))
  (cffi:with-foreign-object (pp '(:struct %:pose-f))
    (setf (cffi:mem-ref pp '(:struct %:pose-f)) pose)
    (with-reference-space-create-info (p
                                       :reference-space-type reference-space-type
                                       :pose-in-reference-space pp)
      (cffi:with-foreign-object (s '%:space)
        (check-result (%:create-reference-space session p s))
        (cffi:mem-ref s '%:space)))))

(defmacro with-reference-space ((space session reference-space-type &rest r)
                                &body body)
  `(let ((,space (create-reference-space ,session ,reference-space-type ,@r)))
     (when *create-verbose*
       (format *debug-io* "~&created space #x~x~%" ,space))
     (unwind-protect
          (progn ,@body)
       (when *create-verbose*
         (format *debug-io* "destroy space #x~x~%" ,space))
       (%:destroy-space ,space))))

(defun create-action-space (session action
                            &key (subaction-path +null-path+) pose)
  (with-pose-f (ppose (or pose (make-pose)))
    (with-action-space-create-info (asci
                                    :action action
                                    :subaction-path subaction-path
                                    :pose-in-action-space ppose)
      (with-returned-handle (p %:space)
        (%:create-action-space session asci p)))))


(import-export %:destroy-space)

;; 7.4. Locating Spaces

(defun locate-space (space base time)
  ;; todo: make velocity optional
  (with-space-velocity (sv :%slots t)
    (with-space-location (sl :%slots t :next sv)
      (check-result (%:locate-space space base time sl))
      (let ((flags %:location-flags)
            (pose %:pose)
            (vflags %:velocity-flags))
        (%::with-bits (flags %:space-location-flags)
          (%::with-bits (vflags %:space-velocity-flags)
            (make-pose+velocity
             :orientation (when (flags :orientation-valid)
                            (getf pose '%:orientation))
             :position (when (flags :position-valid)
                         (getf pose '%:position))
             :orientation-tracked (flags :orientation-tracked)
             :position-tracked (flags :position-tracked)
             :velocity (when (vflags :linear-valid)
                         %:linear-velocity)
             :angular-velocity (when (vflags :angular-valid)
                                 %:angular-velocity))))))))
