(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase2
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
	   #:defclass
	   #:ensure-generic-function
	   #:defgeneric
	   #:defmethod))

