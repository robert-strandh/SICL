(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase3
  (:use #:common-lisp #:aspiring-sicl-clos #:sicl-boot-common)
  (:import-from #:cleavir-code-utilities
		#:proper-list-p
		#:parse-generic-function-lambda-list
		#:parse-ordinary-lambda-list
		#:parse-specialized-lambda-list
		#:required)
  (:import-from #:sicl-additional-conditions
		#:no-such-class-name)
  (:import-from #:sicl-boot-phase1
		;; Miscellaneous functions.
		#:standard-instance-access)
  (:shadowing-import-from #:sicl-boot-phase1
			  #:class-of)
  (:import-from #:sicl-boot-phase2
		#:slot-value-using-class
		#:slot-boundp-using-class
		#:find-ersatz-class
		#:add-ersatz-class
		#:find-ersatz-generic-function
		#:add-ersatz-generic-function)
  (:shadowing-import-from #:sicl-boot-phase2
			  #:make-instance)
  (:shadow
   #:defclass
   #:defgeneric
   #:defmethod
   #:ensure-generic-function)
  (:export
   #:*ensure-class
   #:*ensure-built-in-class
   #:*ensure-generic-function
   #:*ensure-method
   ))
