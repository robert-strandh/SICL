(cl:in-package #:common-lisp-user)

(defpackage #:sicl-structure
  (:use #:common-lisp)
  (:shadow #:defstruct
           #:copy-structure
           #:structure-class
           #:structure-object)
  (:export #:defstruct
           #:copy-structure
           #:structure-class
           #:structure-object))
