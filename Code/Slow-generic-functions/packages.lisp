(cl:in-package #:common-lisp-user)

(defpackage #:fast-generic-functions
  (:use #:common-lisp)
  (:export #:fast-generic-function
           #:seal-domain
           #:sealed-domains
           #:method-properties
           #:inlineable
           #:no-primary-method))

(defpackage #:sealable-metaobjects
  (:use #:common-lisp)
  (:export #:domain-specializers))
