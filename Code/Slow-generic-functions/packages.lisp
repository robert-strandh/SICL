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

(defpackage #:closer-common-lisp
  (:use #:common-lisp)
  (:import-from
   #:sicl-clos
   #:funcallable-standard-object)
  (:export
   #:funcallable-standard-object
   . #.(loop for symbol being each external-symbol of "CL"
             collect (symbol-name symbol))))
