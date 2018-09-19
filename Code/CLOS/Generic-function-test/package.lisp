(cl:in-package #:common-lisp-user)

(defpackage #:sicl-clos
  (:use #:common-lisp)
  (:shadow #:generic-function
           #:standard-generic-function
           #:documentation
           #:make-method
           #:call-method
           #:method-combination
           #:ensure-generic-function
           #:defgeneric)
  (:export #:variant-signature))
