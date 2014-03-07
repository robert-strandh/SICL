(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase3
  (:use #:common-lisp #:aspiring-sicl-clos)
  (:import-from #:sicl-code-utilities
		#:proper-list-p
		#:parse-generic-function-lambda-list
		#:parse-ordinary-lambda-list
		#:parse-specialized-lambda-list
		#:required)
  (:import-from #:sicl-additional-conditions #:no-such-class-name)
  (:shadow
   #:defclass
   #:defgeneric
   #:defmethod
   #:ensure-generic-function)
  (:export
   ))

