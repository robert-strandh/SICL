(cl:in-package #:common-lisp-user)

(defpackage #:sicl-arithmetic
  (:use #:common-lisp)
  (:export
   #:binary-add
   #:binary-sub
   #:binary-mul
   #:binary-div
   #:binary-less
   #:binary-not-greater
   #:binary-greater
   #:binary-not-less
   #:binary-equal))
