(cl:in-package #:common-lisp-user)

(defpackage #:sicl-structure
  (:use #:common-lisp)
  (:local-nicknames (#:mop #:sicl-clos))
  (:export #:defstruct
           #:copy-structure
           #:structure-class
           #:structure-object

           #:structure-slot-definition
           #:structure-slot-definition-read-only
           #:structure-direct-slot-definition
           #:structure-effective-slot-definition))
