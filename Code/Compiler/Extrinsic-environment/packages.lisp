(cl:in-package #:common-lisp-user)

(defpackage #:sicl-extrinsic-environment
  (:use #:common-lisp)
  (:shadow #:load
	   #:function
	   #:catch
	   #:throw
	   #:unwind-protect
	   #:symbol
	   #:symbol-value)
  (:export #:environment
	   #:symbol-value
           #:cst-eval
	   #:load
	   #:load-source-with-environments))
