(cl:in-package #:common-lisp-user)

(defpackage #:sicl-extrinsic-environment
  (:use #:common-lisp)
  (:shadow #:load
	   #:symbol-value)
  (:export #:*environment*))
