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
  (:import-from #:sicl-boot-phase1
		#:specializer-direct-methods
		#:s-direct-methods)
  (:shadow #:defclass
	   #:ensure-generic-function
	   #:defgeneric
	   #:defmethod
	   #:add-method
	   #:remove-method))
