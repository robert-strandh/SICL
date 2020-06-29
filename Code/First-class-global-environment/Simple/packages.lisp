(cl:in-package #:common-lisp-user)

(defpackage #:sicl-simple-environment
  (:use #:common-lisp)
  (:shadow #:type
           #:inline #:notinline
           #:constantp
           #:macro-function #:compiler-macro-function
           #:undefined-function)
  (:export #:simple-environment))
