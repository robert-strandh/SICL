(cl:in-package #:common-lisp-user)

(defpackage sicl-package
  (:use #:common-lisp)
  (:shadow
   .
   #-sicl (#:package
           #:package-error
           #:make-package
           #:intern)))
