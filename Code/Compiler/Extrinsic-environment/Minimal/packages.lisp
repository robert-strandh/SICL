(cl:in-package #:common-lisp-user)

(defpackage #:sicl-minimal-extrinsic-environment
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
           #:import-function-from-host))
