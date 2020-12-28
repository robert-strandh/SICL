(cl:in-package #:common-lisp-user)

(defpackage structure-grammar
  (:use #:common-lisp)
  (:shadow
   #:or
   #:*
   #:+)
  (:export
   #:or
   #:suite))
