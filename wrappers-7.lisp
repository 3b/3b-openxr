(in-package #:3b-openxr-wrappers)

;;; 7. Spaces

;; 7.1. Reference Spaces

(defun get-reference-space-bounds-rect (session type)
  (with-extent-2d-f (e :%slots t)
    (let ((r (check-result (%:get-reference-space-bounds-rect (handle session)
                                                              type e))))
      (if (eql r :space-bounds-unavailable)
          ;; todo: should this return 0,0 with 2nd value for success
          ;; instead?
          :space-bounds-unavailable
          (extent-2d-f %:width %:height)))))


;; 7.2. Action Spaces

;; 7.3. Space Lifecycle

(defun enumerate-reference-spaces (session)
  (with-two-call (i o p %:reference-space-type)
    (%:enumerate-reference-spaces (handle session) i o p)))

(defun create-reference-space (session reference-space-type
                               &key (pose '(%:position #(0 0 0)
                                            %:orientation #(0 0 0 1)))
                                 object-name)
  (cffi:with-foreign-object (pp '(:struct %:pose-f))
    (setf (cffi:mem-ref pp '(:struct %:pose-f)) pose)
    (with-reference-space-create-info (p
                                       :reference-space-type reference-space-type
                                       :pose-in-reference-space pp)
      (with-returned-handle (s %:space :space :name object-name)
        (%:create-reference-space (handle session) p s)))))

(defmacro with-reference-space ((space session reference-space-type &rest r)
                                &body body)
  `(let ((,space (create-reference-space ,session ,reference-space-type ,@r)))
     (when *create-verbose*
       (format *debug-io* "~&created space #x~x~%" (handle ,space)))
     (unwind-protect
          (progn ,@body)
       (when *create-verbose*
         (format *debug-io* "destroy space #x~x~%" (handle ,space)))
       (%:destroy-space (handle ,space)))))

(defun create-action-space (session action
                            &key (subaction-path +null-path+) pose
                              object-name)
  (with-pose-f (ppose (or pose (make-pose)))
    (with-action-space-create-info (asci
                                    :action (handle action)
                                    :subaction-path subaction-path
                                    :pose-in-action-space ppose)
      (with-returned-handle (p %:space :space :name object-name)
        (%:create-action-space (handle session) asci p)))))

(defun destroy-space (space)
  (%:destroy-space (handle space)))

;; 7.4. Locating Spaces

(defun locate-space (space base time)
  ;; todo: make velocity optional
  (with-space-velocity (sv :%slots t)
    (with-space-location (sl :%slots t :next sv)
      (check-result (%:locate-space (handle space) (handle base) time sl))
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
