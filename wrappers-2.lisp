(in-package #:3b-openxr-wrappers)

;;; 2. Fundamentals

;; 2.1 API Version Numbers and Semantics
(import-export %:+current-api-version+
               %:make-version
               %:version-major %:version-minor %:version-patch)


;; 2.2 String Encoding

;; 2.3 Threading Behavior

;; 2.4. Multiprocessing Behavior

;; 2.5. Runtime

;; 2.6. Extensions

;; 2.7. API Layers

;; 2.8. Return Codes


(defvar *check-verbose-quiet* (vector %:event-unavailable %:timeout-expired
                                      %:SPACE-BOUNDS-UNAVAILABLE)
  "Qualified success RESULT codes that are part of normal operation, so
probably shouldn't warn or print anything even with VERBOSE set.")

(defvar *check-verbose* t
  "PRINT something when CHECK-RESULT sees a qualified success value that
isn't in *CHECK-VERBOSE-QUIET*")

(defvar *ignore-invalid-time* t)

(declaim (inline unqualified-success qualified-success succeeded failed))
(defun unqualified-success (result) (zerop result))
(defun qualified-success (result) (plusp result))
(defun succeeded (result) (not (minusp result)))
(defun failed (result) (minusp result))

(defmacro check-result (form)
  (a:with-gensyms (ret)
    `(let ((,ret ,form))
       (cond
         ((unqualified-success ,ret)
          ;; success, do nothing
          )
         ((qualified-success ,ret)
          ;; other success, optionally print or warn
          (if (or (not *check-verbose*)
                  (position ,ret *check-verbose-quiet*))
              ;; do nothing
              nil
              ;; todo: add option to WARN instead? (*check-verbose* = :warn ?)
              (format *debug-io* "~&Qualified Success ~s (~s) from ~a~%  ~s~%"
                      (cffi:foreign-enum-keyword '%::%result ,ret :errorp nil)
                      ,ret
                      ',(car form)
                      ',form)))
         #++
         ((eql %:error-time-invalid ,ret)
          ;; entering the debugger tends to cause this for a bunch of
          ;; subsequent operations during same frame, so might want to
          ;; just print something instead of ERRORing?
          )
         ((failed ,ret)
          (with-simple-restart (continue "continue")
            (xr-error ,ret "~a failed ~s (~s)?~%~s"
                      ',(car form)
                      (cffi:foreign-enum-keyword '%::%result ,ret :errorp nil)
                      ,ret
                      ',form))))
       ,ret)))

;; 2.9 handles
(import-export %:+null-handle+
               %:+null-path+
               %:+null-system-id+)

;; 2.10 object handle types

;; 2.11 Buffer Size Parameters

(defmacro with-two-call ((in out pointer type &key filter filter-pointer)
                         &body call)
  ;; &body is just to get formatting in editor, only accept 1 form
  ;; here
  (assert (= 1 (length call)))
  (setf call (car call))
  (a:with-gensyms (i)
    `(cffi:with-foreign-object (,out :uint32)
       (let ((,in 0)
             (,pointer (cffi:null-pointer)))
         (check-result ,call))
       (let ((,in (cffi:mem-ref ,out :uint32)))
         ,(if (and (typep type '(cons (eql :struct)))
                   (gethash (second type) m::%struct-types%))
              `(cffi:with-foreign-object (,pointer ',type ,in)
                 (loop for ,i below ,in
                       do (setf (cffi:foreign-slot-value
                                 (cffi:mem-aptr ,pointer ',type ,i)
                                 ',type '%:type)
                                ,(gethash (second type) m::%struct-types%))
                          (setf (cffi:foreign-slot-value
                                 (cffi:mem-aptr ,pointer ',type ,i)
                                 ',type '%:next)
                                (cffi:null-pointer)))
                 (check-result ,call)
                 (loop for ,i below ,in
                       collect ,(cond
                                  (filter-pointer
                                   `(,filter-pointer
                                     (cffi:mem-aptr ,pointer ',type ,i)))
                                  (filter
                                   `(,filter (cffi:mem-aref ,pointer ',type ,i)))
                                  (t
                                   `(cffi:mem-aref ,pointer ',type ,i)))))
              `(cffi:with-foreign-object (,pointer ',type ,in)
                 (check-result ,call)
                 (loop for ,i below ,in
                       collect ,(cond
                                  (filter-pointer
                                   `(,filter-pointer
                                     (cffi:mem-aptr ,pointer ',type ,i)))
                                  (filter
                                   `(,filter (cffi:mem-aref ,pointer ',type ,i)))
                                  (t
                                   `(cffi:mem-aref ,pointer ',type ,i))))))))))

(defmacro with-two-call/string ((in out pointer) &body call)
  ;; &body is just to get formatting in editor, only accept 1 form
  ;; here
  (assert (= 1 (length call)))
  (setf call (car call))
  `(cffi:with-foreign-object (,out :uint32)
     (let ((,in 0)
           (,pointer (cffi:null-pointer)))
       (check-result ,call))
     (let ((,in (cffi:mem-ref ,out :uint32)))
       (cffi:with-foreign-pointer-as-string (,pointer ,in :encoding :utf-8)
         (check-result ,call)))))


;; 2.12. Time

(defconstant +nanoseconds+ 1000000000)
(declaim (inline seconds-to-time time-to-seconds))
;; fixme: shorter/betternames for these?
(defun seconds-to-time (n)
  (values (round (* n +nanoseconds+))))
(defun time-to-seconds (n)
  (/ (float n 1d0) +nanoseconds+))

;; 2.13. Duration

;; 2.14. Prediction Time Limits

;; 2.15. Colors

(deftype float-vector (l) `(simple-array single-float (,l)))
(deftype int32-vector (l) `(simple-array (signed-byte 32) (,l)))

(declaim (inline rgba))
;; fixme: decide if this should be a struct or something instead?
(defun rgba (&optional (r 0.0) (g 0.0) (b 0.0) (a 1.0))
  (make-array 4 :element-type '(single-float)
                :initial-contents (list (coerce r 'single-float)
                                        (coerce g 'single-float)
                                        (coerce b 'single-float)
                                        (coerce a 'single-float))))

(declaim (inline r g b a))
(defun r (a) (aref a 0))
(defun g (a) (aref a 1))
(defun b (a) (aref a 2))
(defun a (a) (aref a 3))

;; 2.16. Coordinate System

;; fixme: decide if these should be a struct or something instead?

(declaim (inline x y z w))
(defun x (a) (aref a 0))
(defun y (a) (aref a 1))
(defun z (a) (aref a 2))
(defun w (a) (aref a 3))
(declaim (inline v2f v3f v4f))
(defun v2f (&optional (x 0.0) (y 0.0))
  (make-array 2 :element-type '(single-float)
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float))))

(defun v3f (&optional (x 0.0) (y 0.0) (z 0.0))
  (make-array 3 :element-type '(single-float)
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float)
                                        (coerce z 'single-float))))

(defun v4f (&optional (x 0.0) (y 0.0) (z 0.0) (w 1.0))
  (make-array 4 :element-type '(single-float)
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float)
                                        (coerce z 'single-float)
                                        (coerce w 'single-float))))

(defun quaternion (&optional (x 0.0) (y 0.0) (z 0.0) (w 1.0))
  (make-array 4 :element-type '(single-float)
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float)
                                        (coerce z 'single-float)
                                        (coerce w 'single-float))))

(defstruct pose
  (orientation (quaternion) :type (or null (float-vector 4)))
  (position (v3f) :type (or null (float-vector 3)))
  (orientation-tracked nil :type boolean)
  (position-tracked nil :type boolean))

(defstruct (pose+velocity (:include pose))
  (velocity (v3f) :type (or null (float-vector 3)))
  (angular-velocity (v3f) :type (or null (float-vector 3))))

#++(defun fill-pose-from-pointer ())

(defun make-pose-from-pointer (p flags &optional vflags lv av)
  (cffi:with-foreign-slots ((%:orientation
                             %:position)
                            p (:struct %:pose-f))
    (%::with-bits (flags %:space-location-flags)
      (cond
        ((and vflags
              ;; skip velocity even if present if it has no useful
              ;; contents
              (not (zerop vflags))
              (or lv av))
         (%::with-bits (vflags %:space-velocity-flags)
           (make-pose+velocity
            :position (when (flags :position-valid) %:orientation)
            :orientation (when (flags :orientation-valid) %:orientation)
            :position-tracked (flags :position-tracked)
            :orientation-tracked (flags :orientation-tracked)
            :velocity (when (vflags :linear-valid) lv)
            :angular-velocity (when (vflags :angular-valid) av))))
        (t (make-pose
            :position (when (flags :position-valid) %:position)
            :orientation (when (flags :orientation-valid) %:orientation)
            :position-tracked (flags :position-tracked)
            :orientation-tracked (flags :orientation-tracked)))))))

(defmacro with-pose-f ((pointer-var pose) &body body)
  (a:once-only (pose)
    `(cffi:with-foreign-object (,pointer-var '(:struct %:pose-f))
       (cffi:with-foreign-slots (((%:orientation :pointer %:orientation)
                                  (%:position :pointer %:position))
                                 ,pointer-var (:struct %:pose-f))
         (flet ((vset (p v n)
                  (loop for i below n
                        do (setf (cffi:mem-aref p :float i)
                                 (aref v i))))
                (vzero (p n)
                  (loop for i below n
                        do (setf (cffi:mem-aref p :float i) 0f0))))
           (if (pose-position ,pose)
               (vset %:position (pose-position ,pose) 3)
               (vzero %:position 3))
           (if (pose-orientation ,pose)
               (vset %:orientation (pose-orientation ,pose) 4)
               (vzero %:orientation 4))))
       ,@body)))

;; 2.17. Common Object Types

(declaim (inline width height))
(defun width (a) (aref a 0))
(defun height (a) (aref a 1))

(declaim (inline offset-2d-f offset-2d-i extent-2d-f extent-2d-i))

(defun offset-2d-f (&optional (x 0.0) (y 0.0))
  (make-array 2 :element-type '(single-float)
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float))))

(defun offset-2d-i (&optional (x 0) (y 0))
  (make-array 2 :element-type '(signed-byte 32) :initial-contents (list x y)))

(defun extent-2d-f (&optional (x 0.0) (y 0.0))
  (make-array 2 :element-type '(single-float)
                :initial-contents (list (coerce x 'single-float)
                                        (coerce y 'single-float))))
(defun extent-2d-i (&optional (x 0) (y 0))
  (make-array 2 :element-type '(signed-byte 32) :initial-contents (list x y)))

(defstruct rect-2d-f
  (offset (offset-2d-f) :type (float-vector 2))
  (extent (extent-2d-f) :type (float-vector 2)))

(defstruct rect-2d-i
  (offset (offset-2d-i) :type (int32-vector 2))
  (extent (extent-2d-i) :type (int32-vector 2)))

;; 2.18. Angles

(defstruct (fov (:conc-name angle-))
  (left 0.0 :type single-float)
  (right 0.0 :type single-float)
  (up 0.0 :type single-float)
  (down 0.0 :type single-float))

;; 2.19. Boolean Values

(import-export %::true %::false)

;; 2.20. Events

;; todo: functional API for this (would need to translate output type
;; into some dispatchable types, etc, so probably pretty consy)

(defmacro poll-event-case ((&key edb-var) &body body)
  (let ((edb (or edb-var (gensym "EDB"))))
    (a:with-gensyms (edb-type)
      (flet ((clause (a b)
               ;; like assoc but allows 2 names so we can make shorter
               ;; aliases for the type enums
               (loop for i in body
                     for c = (car i)
                     when (or (eql c a) (eql c b))
                       return (cdr i))))
        (flet ((make-clause (name1 name2 slots type &optional body)
                 `(cffi:with-foreign-slots (,slots
                                            ,edb (:struct ,type))
                    ,@(or (clause name1 name2)
                          body
                          `((when *check-verbose*
                              (format t "unhandled event ~a: ~@{~a:~s.~^ ~}~%"
                                      ,name1
                                      ,@(loop for s in slots
                                              collect `(quote ,s)
                                              collect s))))))))
          `(with-event-data-buffer (,edb)
             ;; don't do anything if it returns :event-unavailable
             (when (unqualified-success
                    (check-result (%:poll-event *instance* ,edb)))
               (let ((,edb-type (cffi:foreign-slot-value
                                 ,edb '(:struct %:event-data-buffer) '%:type)))
                 (case ,edb-type
                   (:type-event-data-session-state-changed
                    (cffi:with-foreign-slots ((%:session %:state %:time)
                                              ,edb (:struct
                                                    %:event-data-session-state-changed))
                      ,@(or
                         (clause :session-state-changed
                                 :type-event-data-session-state-changed)
                         ;; not really useful to have a default here since
                         ;; we can't exit a main loop etc, but leaving it
                         ;; for now for documentation purposes
                         `(case %:state
                            (:idle
                             ;; The initial state after calling xrCreateSession or
                             ;; returned to after calling xrEndSession.
                             )
                            (:ready
                             ;; The application is ready to call xrBeginSession and
                             ;; sync its frame loop with the runtime.
                             (begin-session session :primary-stereo))
                            (:synchronized
                             ;; The application has synced its frame loop with the
                             ;; runtime but is not visible to the user.
                             )
                            (:visible
                             ;; The application has synced its frame loop with the
                             ;; runtime and is visible to the user but cannot receive
                             ;; XR input.
                             )
                            (:focused
                             ;; The application has synced its frame loop with the
                             ;; runtime, is visible to the user and can receive XR
                             ;; input.
                             )
                            (:stopping
                             ;; The application should exit its frame loop and call
                             ;; xrEndSession.
                             (%:end-session session))
                            (:loss-pending
                             ;; The session is in the process of being lost. The
                             ;; application should destroy the current session and
                             ;; can optionally recreate it.
                             )
                            (:exiting
                             ;; The application should end its XR experience and not
                             ;; automatically restart it.
                             )))))
                   (:type-event-data-instance-loss-pending
                    ,(make-clause :instance-loss-pending
                                  :type-event-data-instance-loss-pending
                                  '(%:loss-time)
                                  '%:event-data-instance-loss-pending))
                   (:type-event-data-events-lost
                    ,(make-clause :events-lost
                                  :type-event-data-events-lost
                                  '(%:lost-event-count)
                                  '%:event-data-events-lost
                                  ;; this one should possibly complain
                                  ;; more by default? break would just
                                  ;; make things worse though, so not
                                  ;; sure what it should do
                                  `((when *check-verbose*
                                      (format t "!!events lost! ~s~%"
                                              %:lost-event-count)))))
                   (:type-event-data-interaction-profile-changed
                    ,(make-clause :interaction-profile-changed
                                  :type-event-data-interaction-profile-changed
                                  '(%:session)
                                  '%:event-data-interaction-profile-changed))
                   (:type-event-data-reference-space-change-pending
                    ,(make-clause
                      :reference-space-change-pending
                      :type-event-data-reference-space-change-pending
                      '(%:session %:reference-space-type %:change-time
                        %:pose-valid %:pose-in-previous-space)
                      '%:event-data-reference-space-change-pending
                      `((when *check-verbose*
                          (format t "session #x~x reference space change pending @ ~s~%"
                                  %:session %:change-time)
                          (format t "  type -> ~s, pose-valid ~s~%"
                                  %:reference-space-type %:pose-valid)
                          (format t "  pose in prev space: ~s~%" %:pose-in-previous-space)))))
                   (:type-event-data-perf-settings-ext
                    ,(make-clause :perf-settings-ext
                                  :type-event-data-perf-settings-ext
                                  '(%:domain %:sub-domain
                                    %:from-level %:to-level)
                                  '%:event-data-perf-settings-ext))
                   (:type-event-data-visibility-mask-changed-khr
                    ,(make-clause :visibility-mask-changed-khr
                                  :type-event-data-visibility-mask-changed-khr
                                  '(%:session
                                    %:view-configuration-type %:view-index)
                                  '%:event-data-visibility-mask-changed-khr))
                   ;; todo
                   (:type-event-data-display-refresh-rate-changed-fb
                    ,(make-clause :display-refresh-rate-changed-fb
                                  :type-event-data-display-refresh-rate-changed-fb
                                  '(%:from-display-refresh-rate
                                    %:to-display-refresh-rate)
                                  '%:event-data-display-refresh-rate-changed-fb))
                   (:type-event-data-main-session-visibility-changed-extx
                    ,(make-clause :main-session-visibility-changed-extx
                                  :type-event-data-main-session-visibility-changed-extx
                                  '(%:visible %:flags)
                                  '%:event-data-main-session-visibility-changed-extx))
                   (:type-event-data-marker-tracking-update-varjo
                    ,(make-clause :marker-tracking-update-varjo
                                  :type-event-data-marker-tracking-update-varjo
                                  '(%:marker-id
                                    %:is-active %:is-predicted %:time)
                                  '%:event-data-marker-tracking-update-varjo))
                   (:type-event-data-space-erase-complete-fb
                    ,(make-clause :space-erase-complete-fb
                                  :type-event-data-space-erase-complete-fb
                                  '(%:request-id %:result %:space
                                    %:uuid %:location)
                                  '%:event-data-space-erase-complete-fb))
                   (:type-event-data-space-list-save-complete-fb
                    ,(make-clause :space-list-save-complete-fb
                                  :type-event-data-space-list-save-complete-fb
                                  '(%:request-id %:result)
                                  '%:event-data-space-list-save-complete-fb))
                   (:type-event-data-space-query-complete-fb
                    ,(make-clause :space-query-complete-fb
                                  :type-event-data-space-query-complete-fb
                                  '(%:request-id %:result)
                                  '%:event-data-space-query-complete-fb))
                   (:type-event-data-space-query-results-available-fb
                    ,(make-clause :space-query-results-available-fb
                                  :type-event-data-space-query-results-available-fb
                                  '(%:request-id)
                                  '%:event-data-space-query-results-available-fb))
                   (:type-event-data-space-save-complete-fb
                    ,(make-clause :space-save-complete-fb
                                  :type-event-data-space-save-complete-fb
                                  '(%:request-id %:result %:space
                                    %:uuid %:location)
                                  '%:event-data-space-save-complete-fb))
                   (:type-event-data-space-set-status-complete-fb
                    ,(make-clause :space-set-status-complete-fb
                                  :type-event-data-space-set-status-complete-fb
                                  '(%:request-id %:result %:space
                                    %:uuid %:component-type %:enabled)
                                  '%:event-data-space-set-status-complete-fb))
                   (:type-event-data-space-share-complete-fb
                    ,(make-clause :space-share-complete-fb
                                  :type-event-data-space-share-complete-fb
                                  '(%:request-id %:result)
                                  '%:event-data-space-share-complete-fb))
                   (:type-event-data-spatial-anchor-create-complete-fb
                    ,(make-clause :spatial-anchor-create-complete-fb
                                  :type-event-data-spatial-anchor-create-complete-fb
                                  '(%:request-id %:result %:space %:uuid)
                                  '%:event-data-spatial-anchor-create-complete-fb))
                   (:type-event-data-vive-tracker-connected-htcx
                    ,(make-clause :vive-tracker-connected-htcx
                                  :type-event-data-vive-tracker-connected-htcx
                                  '(%:paths)
                                  '%:event-data-vive-tracker-connected-htcx))
                   (otherwise
                    (,@ (or (clause 'otherwise t)
                            `(when *check-verbose*
                               (break "unknown event type ~s?" ,edb-type))))))))))))))


;; 2.21. System resource lifetime
