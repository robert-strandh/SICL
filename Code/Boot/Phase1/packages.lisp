(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase1
  (:use #:common-lisp)
  (:export #:environment
	   #:compilation-environment))
