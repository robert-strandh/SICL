(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:shadow . #1=(#:fdefinition
		 #:symbol-function))
  (:export . #1#))
