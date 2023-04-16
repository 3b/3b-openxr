(in-package #:3b-openxr-parse-spec2)

(defvar *package-name* "3b-openxr")
(defvar *bindings-package-name* "3b-openxr-bindings")
(defvar *core-definer* 'defcfun)
(defvar *ext-definer* 'defextfun)

(defvar *ext-enum-base* (floor 1e10))
(defvar *ext-enum-block* 1000)


(defvar *spec-dir* (asdf:system-relative-pathname '3b-openxr "spec/"))

(defvar *xml* (cxml:parse-file
               (merge-pathnames "xr.xml" *spec-dir*)
               (cxml:make-whitespace-normalizer
                (stp:make-builder))))


(defparameter *parser* ())
(defparameter *path* (list :/))
(defparameter *node* nil)
(defparameter *node-attribs* nil)
(defparameter *parser-slots* nil)
(defparameter *parser-class* nil)
(defparameter *header-comments* nil)

(defun translate-node-name (n)
  (let ((r (find-symbol (string-upcase (substitute #\- #\_ n))
                        (find-package '#:3b-openxr-parse-spec2))))
    (assert r)
    r))

;; not generated from xml schema, since we rearrange things a bit when
;; generating types, and also since we should be thinking about how
;; the generated code should change when schema changes anyway
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; just used for parser-generator macro
  (defvar *parser-classes* nil)
  (defvar *parser-parsers* nil))

;; these are mostly just to get indentation in editor
(defmacro tag (name (&key) &body body)
  (declare (ignore name body)))
(defmacro attrib-tag (name attrib (&key) &body body)
  (declare (ignore name attrib body)))
;;(defmacro attrib (name (&key) &body body))

(defun path-keyword (nodes)
  (a:make-keyword (format nil "~(~{/~a~}~)" (reverse nodes))))

#++(defvar *parsers* (list (make-hash-table :test 'equal)))

(defclass parser ()
  ;; thunks called at start and end of parsing a node
  ((start :initarg :start :reader start)
   (finish :initarg :finish :reader finish)
   ;; name of class to create (if any)
   (class-name :initarg :class-name :reader class-name)
   ;; slot initarg in parent containing this node (usually just
   ;; keyword version of class-name?)
   (parent-slot :initarg :parent-slot :reader parent-slot)
   ;; predicate indicating this parser doesn't apply to a specific
   ;; node (defaults to returning NIL without checking node type, so
   ;; doesn't work on arbitrary nodes)
   (filter :initarg :filter :reader filter :initform (constantly nil))
   ;; flag indicating text should be stored for nodes
   (text :initarg :text :initform nil :reader text)
   ;; storage for initargs of class being built while processing
   ;; children (keyword->value, so can convert to plist and then apply
   ;; to make-instance)
   (slots :initform (make-hash-table) :reader slots)
   ;; parsers for attrib values
   (attribs :initform (make-hash-table) :reader attribs :initarg :attribs)
   ;; parsers for child elements
   (parsers :initform (make-hash-table) :reader parsers :initarg :parsers)))

;; base class for instances created by parser
(defclass parsed ()
  (;; any comments before this node
   (header-comment :initarg :header-comment :reader header-comment)))
;; mixins for various slots that need extra parsing
(defclass has-name ()
  ;; translate NAME slot into lisp name (use typecase on object to
  ;; decide which translation if any to use?)
  ((raw-name :reader raw-name :initform nil)))
(defclass has-type ()
  ((raw-type :reader raw-type :initform nil)))
(defclass has-alias ()
  ((raw-alias :reader raw-alias :initform nil)))
(defclass has-extends()
  ((raw-extends :reader raw-extends :initform nil)))
(defclass has-len ()
  ;; LEN is comma separated list of (slot name or "null-terminated")
  ((raw-len :reader raw-len :initform nil)))
(defclass has-errorcodes ()
  ;; ERRORCODES is comma separated list of (negative) xrResult enums
  ((raw-error-codes :reader raw-error-codes :initform nil)))
(defclass has-successcodes ()
  ;; ERRORCODES is comma separated list of (positive) xrResult enums
  ((raw-success-codes :reader raw-success-codes :initform nil)))

(defclass has-comment ()
  ())



(defun get-current-enums-name ()
  (assert (eql :enums (parent-slot (second *parser*))))
  (let ((name (gethash :name (parent-parser-slots))))
    (assert name)
    name))

(defun get-node-attrib (attrib &key error)
  (let ((a (getf *node-attribs* attrib)))
    (when (and error (not a))
      (error "missing attribute ~s?~%~{ ~s ~s~%~}" attrib *node-attribs*))
    a))


(defun add-mixins (slots)
  (loop for s in slots
        when (case s
               (name 'has-name)
               (len 'has-len)
               (type 'has-type)
               (alias 'has-alias)
               (extends 'has-extends)
               (errorcodes 'has-errorcodes)
               (successcodes 'has-successcodes)
               (comment 'has-comment))
          collect it))

(defparameter *root-parser*
  (make-instance 'parser
                 :start #'identity
                 :finish (lambda (n)
                           (declare (ignore n))
                           (gethash :registry (current-parser-slots)))
                 :class-name nil))

(defun no-finish () (lambda (n) (declare (ignore n))))
(defparameter *comment-parser*
  (make-instance 'parser
                 :start (lambda ()
                          (push (format-comment (xps *node*)) *header-comments*))
                 :finish (no-finish)
                 :class-name :comment))

#++
(setf (gethash :registry (parsers *root-parser*))
      (make-instance 'parser :start nil :finish nil
                             :class-name 'registry))

(defun make-keyword (name &key prefix)
  (if prefix
      (a:format-symbol :keyword "~a/~a"
                       (string-upcase prefix)
                       (string-upcase name))
      (a:make-keyword (string-upcase name))))

(defun attrib-plist (node)
  (loop for a in (stp:list-attributes node)
        for v = (stp:attribute-value node (stp:local-name a))
        collect (translate-node-name (stp:local-name a))
        collect v))

(defun xps (node)
  (let ((s (string-trim '(#\space #\tab #\newline) (xpath:string-value node))))
    (unless (string= s "") s)))

(defun format-comment (s &key prefix)
  (with-output-to-string (*standard-output*)
    (with-input-from-string (ss s)
      (loop for l = (read-line ss nil nil)
            while l
            for tl = (string-right-trim " " l)
            do (format t "~@[~a~];;~@[ ~a~]~%" prefix
                       (unless (string= tl "") tl))))))

(defun select-parser (name)
  (let ((parser (a:ensure-list
                 ;; find parser(s) for node type
                 (or (gethash name (parsers (or (car *parser*)
                                                *root-parser*)))
                     (if (eql name 'comment)
                         *comment-parser*
                         (error "no parser for node ~s?" name))))))
    ;; find a parser that applies to this specific node
    (loop for p in parser
          unless (funcall (filter p) *node*)
            return p)))

(defun parse1 (node)
  (declare (optimize debug))
  (let* ((*node* node)
         (*node-attribs* (attrib-plist *node*))
         (name (translate-node-name (stp:local-name node)))
         (parser (select-parser name))
         #+_+(*parser-slots* (or *parser-slots* (slots parser)))
         (*parser* (cons parser *parser*))
         (*path* (cons name *path*))
         (comment (shiftf *header-comments* nil))
         (*header-comments* nil)
         (*parser-class* nil))
    (when (eql name 'registry)
      (clrhash (slots *root-parser*)))
    (assert parser () "couldn't find parser for ~s at ~s?~%~s"
            name *path*
            *node-attribs*)
    ;; start class
    (funcall (start parser))
    (when (class-name parser)
      (setf *header-comments* nil))
    ;; parse attribs
    (let ((attribs *node-attribs*))
      (format t "attribs =~%~{ ~s ~s~%~}" attribs)
      (loop for (a v) on attribs by #'cddr
            for f = (gethash a (attribs parser))
            do (if f
                   (funcall f a v)
                   (error "unknown attribute ~s parsing ~s~%~a"
                          a *path* *parser*))))
    ;; parse children
    (unless (eql name 'comment)
      (cxml-stp:do-children (x node)
        (typecase x
          (cxml-stp:text
           #++ (when (xps x) (unless *noprint* (format t " text ~s~%" (xps x)))))
          (cxml-stp:comment
           (push (format-comment (xps x)) *header-comments*))
          (t
           (parse1 x)))))
    ;; finish node
    (let ((pc (when *parser-class*
                (format t "~&make instance ~s~%~{ ~s ~s~%~}"
                        *parser-class*
                        (a:hash-table-plist (current-parser-slots)))
                (apply #'make-instance *parser-class*
                       :header-comment comment
                       (a:hash-table-plist (current-parser-slots))))))
      (when pc
        (setf *header-comments* nil
              comment nil))
      (funcall (finish parser) pc)
      (when (hash-table-p (current-parser-slots))
        (clrhash (current-parser-slots))))))

(defun collect-enum-names (xml)
  (clrhash *known-enum-names*)
  (xpath:do-node-set (enums (xpath:evaluate "/registry/enums" xml))
    (let ((pname (getf (attrib-plist enums) 'name)))
      (assert pname)
      (xpath:do-node-set (enum (xpath:evaluate "enum" enums))
        (let* ((name (getf (attrib-plist enum) 'name))
               (ename (make-enum-name name pname)))
          (assert name)
          (assert ename)
          (assert (not (gethash name *known-enum-names*)) ()
                  "duplicate enum ~s (~s) in ~s?" ename name pname)
          (setf (gethash name *known-enum-names*) ename)))))
  (xpath:do-node-set (enum (xpath:evaluate "/registry/extensions/extension/require/enum" xml))
    (let* ((name (getf (attrib-plist enum) 'name))
           (extends (getf (attrib-plist enum) 'extends))
           (ename (when name (make-enum-name name extends))))
      (assert name)
      ;;(assert extends)
      (assert ename)
      (assert (not (gethash name *known-enum-names*)) ()
              "duplicate enum ~s (~s) in ~s?" ename name extends)
      (setf (gethash name *known-enum-names*) ename))))

(defun collect-suffixes (xml)
  (setf *suffix-list* nil)
  (setf *suffix-regex* nil)

  (xpath:do-node-set (tag (xpath:evaluate "/registry/tags/tag" xml))
    (let ((name (getf (attrib-plist tag) 'name)))
      (assert name)
      (push name *suffix-list*))))

(defun parse (node)
  (let ()
    ;; just setting some of these instead of binding so we can keep
    ;; them around after parsing(either for debugging or for later
    ;; use)
    (collect-suffixes *xml*)
    (collect-enum-names *xml*)
    (parse1 node)
    (gethash :registry (slots *root-parser*))))

#++
(xpath:map-node-set->list #'parse (xpath:evaluate "/*" *xml*))
#++
*root-parser*

(defun current-parser ()
  (car *parser*))

(defun current-parent-slot ()
  (parent-slot (current-parser)))

(defun current-parser-slots ()
  (slots (current-parser)))

(defun parent-parser-slots (&key (depth 1))
  (slots (if (nthcdr depth *parser*)
             (car (nthcdr depth *parser*))
             *root-parser*)))

(defun default-parse-attrib (attrib value)
  (assert value)
  (let ((pps (current-parser-slots)))
    #++(format t "attrib ~s = ~s~%" attrib value)
    (etypecase pps
      (vector
       (vector-push-extend value pps))
      (hash-table
       #++(format t "pps = ~s~%" (a:hash-table-plist pps))
       (let ((k (a:make-keyword attrib)))
         (unless (not (gethash k pps))
           (break "duplicate attrib ~s = ~s?~%~{ ~s ~s~%~}"
                  attrib value (a:hash-table-plist pps)))
         (setf (gethash k pps) value))))))

(defmethod generate-parser-1 ((tag (eql 'tag)) parent rest)
  (destructuring-bind (name (&key unique class attribs optional index
                               flatten text)
                       &rest children)
      rest
    (declare (ignorable optional index))
    (let* ((*parser-slots* #++(make-hash-table)
                           (if flatten
                               *parser-slots*
                               (make-hash-table)))
           (r (list :attribs
                    (a:alist-hash-table
                     (loop for i in attribs
                           collect (cons i 'default-parse-attrib))))))
      (generate-parser children (cons tag parent))
      (when (eql class t) (setf class name))
      (progn                            ;unless flatten
        (setf r (list* :parsers *parser-slots* r)))
      #++(when (text (current-parser))
           (setf r (list* :text (xps *node*) r)))
      (when class
        (let ((slots (append attribs (a:hash-table-keys *parser-slots*))))
          (when text (push 'text slots))
          (push `(defclass ,class (parsed ,@ (add-mixins slots))
                   (,@ (loop for s in slots
                             for k = (a:make-keyword s)
                             collect (list s :initarg k :reader s))))
                *parser-classes*)))

      (if flatten
          (list* :start (lambda ()
                          #++(setf (slot-value (current-parser) 'slots)
                                   (parent-parser-slots))
                          (when (hash-table-p
                                 (current-parser-slots))
                            (clrhash (current-parser-slots)))
                          (format t "start0 ~s~%" name))
                 :finish (lambda (n)
                           (declare (ignore n))
                           (loop for k being the hash-keys
                                   of (current-parser-slots)
                                     using (hash-value v)
                                 do (Assert
                                     (not (gethash k (parent-parser-slots))))
                                    (setf (gethash k (parent-parser-slots))
                                          v))
                           #+=(break "flat ~s~%~s~%~s~%" n
                                     (a:hash-table-plist (current-parser-slots))
                                     (a:hash-table-plist (parent-parser-slots))))
                 :class-name nil
                 r)
          (list* :start (labels ((unique ()
                                   (when (cdr *parser*)
                                     (let ((s (gethash (current-parent-slot)
                                                       (parent-parser-slots))))
                                       (assert (null s)))))
                                 (not-unique ()
                                   (when (cdr *parser*)
                                     (unless (typep (parent-parser-slots)
                                                    'vector)
                                       (let ((s (gethash (current-parent-slot)
                                                         (parent-parser-slots))))
                                         (assert (or (null s)
                                                     (typep s 'vector)))
                                         (unless (typep s 'vector)
                                           (format t "1set parent slot to vec~%")
                                           (setf (gethash (current-parent-slot)
                                                          (parent-parser-slots))
                                                 (make-array 1 :adjustable t
                                                               :fill-pointer 0)))))))
                                 (common ()
                                   (if unique (unique) (not-unique))
                                   (if class
                                       (progn
                                         (setf *parser-class* (class-name
                                                               (current-parser)))
                                         (when (hash-table-p
                                                (current-parser-slots))
                                           (clrhash (current-parser-slots))))
                                       (setf *parser-class* nil))
                                   (when text
                                     (setf (gethash :text (current-parser-slots))
                                           (xps *node*)))))
                          (cond
                            ((and unique (not (or class text)))
                             (lambda ()
                               (format t "start-u!c ~s~%" name)
                               (common)
                               (format t "2set parent slot to vec~%")
                               (setf (slot-value (current-parser) 'slots)
                                     (make-array 1 :adjustable t :fill-pointer 0))))
                            #++((and unique class)
                                (lambda ()
                                  (format t "~&start-uc ~s~%" name)
                                  (setf *parser-class* (class-name (current-parser)))
                                  (clrhash (current-parser-slots))
                                  (cond
                                    ((not (cdr *parser*)))
                                    ((hash-table-p (parent-parser-slots))
                                     (format t "sset ~s in ~s to ~s~%"
                                             (parent-slot (current-parser))
                                             (parent-parser-slots)
                                             (xps *node*))
                                     #++(setf (gethash (current-parent-slot)
                                                       (parent-parser-slots))
                                              nil))
                                    (t (break " ?")))))
                            (class
                             (lambda ()
                               (progn
                                 (format t "start1 ~s~%" name)
                                 (common)
                                 #++(when *parser-class*
                                      (clrhash (current-parser-slots))))))
                            (text
                             (lambda ()
                               (progn
                                 (format t "start-t ~s~%" name)
                                 (common))))
                            (unique
                             (lambda ()
                               (progn
                                 (format t "start3 ~s~%" name)
                                 (common))))
                            ((not unique)
                             (lambda ()
                               (format t "~&start-!u ~s~%" name)
                               (common)
                               (cond
                                 ((not (cdr *parser*)))
                                 ((hash-table-p (parent-parser-slots))
                                  (format t "3set parent slot to vec~%")
                                  (setf (gethash (current-parent-slot)
                                                 (parent-parser-slots))
                                        (make-array 1 :adjustable t :fill-pointer 0)))
                                 (t (break " ?")))))
                            #++(t (break "todo"))))
                 :finish (cond
                           ((and class (not unique))
                            (lambda (n)
                              (format t "end1 ~s~%" name)
                              (etypecase (parent-parser-slots)
                                ;; parent might not have a class, in
                                ;; which case we just push directly into
                                ;; a vector containing siblings of this
                                ;; node
                                (vector
                                 (vector-push-extend n (parent-parser-slots)))
                                ;; if parent has a class, slot for
                                ;; this node should contain a similar
                                ;; vector
                                (hash-table
                                 (let ((v (gethash (current-parent-slot)
                                                   (parent-parser-slots))))
                                   (assert (typep v 'vector))
                                   (vector-push-extend n v))))))
                           ((and class unique)
                            (lambda (n)
                              (format t "end2 ~s~%" name)
                              (assert (not (gethash (current-parent-slot)
                                                    (parent-parser-slots))))
                              (assert n)
                              (setf (gethash (current-parent-slot)
                                             (parent-parser-slots))
                                    n)))
                           ((and text (not unique))
                            (lambda (n)
                              (format t "end3 ~s~%" name)
                              (assert (not n))
                              (let ((node-text (if (eql text t)
                                                   (xps *node*)
                                                   (get-node-attrib text :error t))))
                                (format t " ~s +> ~s~%"
                                        (parent-slot (current-parser))
                                        node-text)
                                (vector-push-extend
                                 node-text
                                 (gethash (parent-slot (current-parser))
                                          (parent-parser-slots))))))
                           ((and text unique)
                            (lambda (n)
                              (format t "end4 ~s~%" name)
                              (assert (not n))
                              (let ((node-text (if (eql text t)
                                                   (xps *node*)
                                                   (get-node-attrib text :error t))))
                                (format t "fset ~s in ~s to ~s~%"
                                        (parent-slot (current-parser))
                                        (parent-parser-slots)
                                        node-text)
                                (assert (not (gethash (parent-slot
                                                       (current-parser))
                                                      (parent-parser-slots))))

                                (setf (gethash (parent-slot (current-parser))
                                               (parent-parser-slots))
                                      node-text))))
                           (unique
                            (lambda (n)
                              (format t "end5 ~s~%" name)
                              (assert (not n))
                              (assert (not (gethash (parent-slot (current-parser))
                                                    (parent-parser-slots))))
                              (setf (gethash (parent-slot (current-parser))
                                             (parent-parser-slots))
                                    (slots (current-parser)))))
                           (t
                            (error "?")))
                 :class-name class
                 :parent-slot (a:make-keyword name #++(or class name))
                 r))

      #++(if flatten
             (progn
               (reduce 'append r :from-end t))
             (cond
               (class
                (when (eql class t) (setf class name))
                #++(list :class (if (eql class t) name class))
                (push (list :class class
                            :r r)
                      *parser-classes*))
               (unique
                (setf unique (if (eql unique t) name unique))
                (append
                 (list :slots (list unique))
                 (when optional (list :optional optional))
                 (when index (list :index index))
                 (when adjustable (list :adjustable adjustable)))))))))

(defmethod generate-parser-1 ((tag (eql 'attrib-tag)) parent rest)
  (destructuring-bind (name attrib (&key attribs text)
                       &rest children)
      rest
    (let* ((*parser-slots* (make-hash-table))
           (r (list :attribs
                    (a:alist-hash-table
                     (loop for i in attribs
                           collect (cons i 'default-parse-attrib)))))
           (filter nil)
           (class nil))
      (generate-parser children (cons tag parent))
      (setf r (list* :parsers *parser-slots* r))
      (if (consp attrib)
          (setf filter (lambda (node)
                         (assert (eql node *node*))
                         (let* ((a *node-attribs*)
                                (v (getf a (car attrib))))
                           #++ (break "~s ~s ~s @ ~s~%"
                                      attrib (cadr attrib) v a)
                           (not (and v
                                     (eql (cadr attrib)
                                          (translate-node-name v))))))
                class (second attrib))
          (setf filter (lambda (node)
                         (assert (eql node *node*))
                         (not (getf *node-attribs* attrib)))
                class attrib))
      (setf class (a:format-symbol '#:3b-openxr-parse-spec2 "~a/~a" name class))
      (setf r (list* :filter filter r))
      (let ((slots (append attribs (a:hash-table-keys *parser-slots*))))
        (when text (push 'text slots))
        (push `(defclass ,class (parsed ,@ (add-mixins slots))
                 (,@ (loop for s in slots
                           for k = (a:make-keyword s)
                           collect (list s :initarg k :reader s))))
              *parser-classes*))

      (list* :start (lambda ()
                      (progn
                        (format t "start1 ~s ~s~%" name attrib)
                        (setf *parser-class* (class-name
                                              (current-parser)))
                        (when (and (cdr *parser*)
                                   (not (typep (parent-parser-slots)
                                               'vector)))
                          (let ((s (gethash (current-parent-slot)
                                            (parent-parser-slots))))
                            (assert (or (null s)
                                        (typep s 'vector)))
                            (unless (typep s 'vector)
                              (setf (gethash (current-parent-slot)
                                             (parent-parser-slots))
                                    (make-array 1 :adjustable t
                                                  :fill-pointer 0)))))
                        (when *parser-class*
                          (clrhash (current-parser-slots)))
                        (when text
                          (setf (gethash :text (current-parser-slots))
                                (xps *node*)))))
             :finish (lambda (n)
                       (format t "end1 ~s~%" name)
                       (vector-push-extend n (parent-parser-slots))
                       #++(vector-push-extend n (gethash (current-parent-slot)
                                                         (parent-parser-slots))))

             :class-name class
             :parent-slot (a:make-keyword class)
             r))))

(defun generate-parser (nodes &optional parent)
  (loop for (tag . n) in nodes
        for *parser* = nil
        for r = (generate-parser-1 tag (cons (car n) parent) n)
        for p = (apply #'make-instance 'parser r)
        do (format t "~&make parser ~s:~%~{ ~s ~s~%~}"
                   (cons (car n) parent) r)
        collect (push p (gethash (car n) *parser-slots* nil))))


(defmacro schema (&rest nodes)
  (let ((*parser-classes* nil)
        (*parser-parsers* nil)
        (*parser-slots* (parsers *root-parser*)))
    (generate-parser nodes)
    `(progn
       (when (zerop (hash-table-count (parsers *root-parser*)))
         ;; this has side effects, so run it again at load time if
         ;; needed
         (let ((*parser-classes* nil)
               (*parser-parsers* nil)
               (*parser-slots* (parsers *root-parser*)))
           (generate-parser ',nodes)))
       ,@(print *parser-classes*))))


#++
(xpath:map-node-set->list #'parse (xpath:evaluate "/*" *xml*))
#++
(gethash :registry (slots *root-parser*))
#++
(defparameter *spec* (gethash :registry (slots *root-parser*)))
