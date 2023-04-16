(in-package #:3b-openxr-parse-spec)

;; this needs to be separate from definitions in parse-spec since it
;; has side-effects at macroexpansion time, and easier to just put it
;; in a separate file than fix it
(eval-when (:compile-toplevel :load-toplevel :execute)
  (schema
   (tag registry (:unique t
                  ;; generate a class containing slots for attributes
                  ;; and child tags
                  :class t)
     (tag comment (:text t))
     ;; assuming for now toplevel groups are unique aside from <enums>,
     ;; so parent slot will contain value directly. if not :unique,
     ;; parent slot will contain a generic container class with an
     ;; optional preceding comment and a vector of this type
     (tag vendorids (:unique t)
       (tag vendorid (;; expect (some of) these attribs, store as
                      :attribs (name id comment)
                      :class t)))

     (tag tags (:unique t)
       (tag tag (:attribs (name author contact)
                 :class t)))

     (tag types (:unique t)
       (attrib-tag type requires
           (:attribs (name requires)))
       (attrib-tag type (category include)
           (:attribs (name category) :text t))
       (attrib-tag type (category define)
           (:attribs (category) :text t)
         (tag name (:unique t :text t))
         (tag type (:unique t :optional t :text t)))
       (attrib-tag type (category basetype)
           (:attribs (category))
         (tag name (:unique t :text t))
         (tag type (:unique t :text t)))
       (attrib-tag type (category bitmask)
           (:attribs (bitvalues category))
         (tag name (:unique t :text t))
         (tag type (:unique t :text t)))
       (attrib-tag type (category handle)
           (:attribs (category parent))
         (tag name (:unique t :text t))
         (tag type (:unique t :text t)))
       (attrib-tag type (category enum)
           (:attribs (name category)))
       (attrib-tag type (category funcpointer)
           (:attribs (category requires) :text t)
         (tag name (:unique t :text t))
         (tag type (:text t)))
       (attrib-tag type (category struct)
           (:attribs (name category protect parentstruct structextends mayalias
                           returnedonly alias))
         (tag member (:attribs (optional values len noautovalidity)
                      :class t
                      :text t)
           (tag type (:unique t :text t))
           (tag enum (:unique t :text t))
           (tag name (:unique t :text t)))))

     (tag enums (:attribs (name type comment)
                 :class t
                 :index name)
       (tag enum (:attribs (name value comment alias bitpos)
                  :class t))
       (tag unused (:attribs (start) :class t)))

     (tag commands (:unique t)
       (tag command (:attribs (successcodes errorcodes)
                     :class t)
         (tag proto (:unique t
                     ;; don't make a slot for this, put children in
                     ;; parent
                     :flatten t)
           (tag name (:unique t :text t))
           (tag type (:unique t :text t)))
         (tag param (:attribs (optional len externsync)
                     :class t
                     :text t)
           (tag name (:unique t :text t))
           (tag enum (:unique t :text t))
           (tag type (:unique t :text t)))
         (tag implicitexternsyncparams (:class t)
           (tag param (:text t))))
       ;; fixme: fix order of checking attrib so first is checked first
       (attrib-tag command alias (:attribs (name alias))))
     (tag interaction-profiles (:unique t)
       (tag interaction-profile (:class t
                                 :attribs (name title))
         (tag user-path (:attribs (path) :text t))
         (tag component (:attribs (user-path subpath type system)
                         :class t))))
     (tag feature (:class t
                   :attribs (api name number))
       (tag require (:class t
                     :attribs (api name number comment))
         (tag type (:attribs (name) :text name))
         (tag enum (:attribs (name) :text name))
         (tag command (:attribs (name) :text name))
         (tag interaction-profile (:attribs (name) :text name))))
     (tag extensions (:unique t)
       (tag extension (:class t
                       :attribs (name number type supported protect requires
                                      provisional promotedto))
         (tag require (:class extension/require
                       :attribs (extension))
           (tag enum (:attribs (name value offset dir extends comment alias
                                     bitpos)
                      :text t
                      :class enum/ext))
           (tag type (:attribs (name) :text name))
           (tag command (:attribs (name) :text name))
           (tag interaction-profile (:attribs (name) :text name))
           (tag extend (:attribs (interaction-profile-path) :class t)
             (tag component (:attribs (subpath type) :class t)))))))))
