(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase1
  (:use #:common-lisp)
  (:import-from #:sicl-code-utilities
		#:proper-list-p
		#:parse-generic-function-lambda-list
		#:parse-ordinary-lambda-list
		#:parse-specialized-lambda-list
		#:required)
  (:import-from #:sicl-additional-conditions #:no-such-class-name)
  (:shadow #:function
	   #:class
	   #:standard-class
	   #:built-in-class
	   #:standard-object
	   #:generic-function
	   #:standard-generic-function
	   #:method
	   #:standard-method
	   #:method-combination
	   #:defclass)
  (:export #:standard-class
	   #:built-in-class
	   #:standard-generic-function
	   #:standard-method
	   #:specializer-direct-methods
	   #:s-direct-methods
	   #:generic-function-methods
	   #:gf-methods
	   #:method-generic-function
	   #:m-generic-function
	   #:method-specializers
	   #:specializer-profile))
