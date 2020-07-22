(cl:in-package #:common-lisp-user)

(defpackage #:target
  (:use #:common-lisp)
  (:shadow
   #:defmacro
   #:when
   #:unless
   #:do
   #:dolist
   #:multiple-value-bind))
