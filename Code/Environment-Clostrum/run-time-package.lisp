(cl:in-package #:common-lisp-user)

(defpackage #:sicl-run-time
  (:use #:common-lisp)
  (:shadow
   . #1=(#:boundp
         #:symbol-value))
  (:export
   . #1#))
