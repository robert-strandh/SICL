(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase3
  (:use #:common-lisp #:aspiring-sicl-clos)
  (:import-from #:sicl-code-utilities
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
		#:slot-boundp-using-class)
  (:shadow
   #:defclass
   #:defgeneric
   #:defmethod
   #:ensure-generic-function
   #:make-instance)
  (:export
   ))

