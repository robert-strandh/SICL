(cl:in-package #:common-lisp-user)

(defpackage #:sicl-minimal-extrinsic-environment
  (:use #:common-lisp)
  (:shadow #:load
	   #:function
	   #:catch
	   #:throw
	   #:unwind-protect
	   #:symbol
	   #:symbol-value
           #:trace
           #:untrace)
  (:export #:environment
           #:symbol-value
           #:import-function-from-host
           #:import-package-from-host
           #:host-load
           #:cst-load-file
           #:trace
           #:untrace))
