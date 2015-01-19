(cl:in-package #:common-lisp-user)

(defpackage #:sicl-extrinsic-hir-compiler
  (:use #:common-lisp)
  (:shadow #:unwind-protect
	   #:symbol
	   #:function
	   #:catch
	   #:throw
	   #:eval
	   #:load
	   #:symbol-value)
  (:export
   #:eval
   #:load
   ))
