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
  (:shadowing-import-from
   #:sicl-boot-phase1
   #:ensure-generic-function)
  (:import-from
   #:sicl-boot-phase1
   ;; Names of classes
   #:funcallable-standard-class
   #:standard-reader-method
   #:standard-writer-method
   #:eql-specializer
   ;; Miscellaneous functions.
   #:heap-instance-p
   #:standard-instance-access
   #:ensure-class
   #:ensure-built-in-class
   #:ensure-method)
  (:shadow #:defclass
	   #:defgeneric
	   #:defmethod
	   #:add-method
	   #:remove-method
	   #:compute-applicable-methods))
