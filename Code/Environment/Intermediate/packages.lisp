(cl:in-package #:common-lisp-user)

(defpackage #:sicl-environment
  (:use #:common-lisp)
  (:shadow . #1=(#:fboundp
		 #:fdefinition
		 #:symbol-function
		 #:find-class
		 #:macroexpand-1))
  (:export . #1#))
