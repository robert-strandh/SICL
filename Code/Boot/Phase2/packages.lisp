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
  (:import-from
   #:sicl-boot-phase1
   ;; Names of classes
   #:standard-reader-method
   #:standard-writer-method
   #:eql-specializer
   ;; Accessors for specializer metaobjects.
   #:specializer-direct-methods
   #:s-direct-methods
   #:class-direct-subclasses
   #:class-precedence-list
   #:effective-slots
   #:unique-number
   #:eql-specializer-object
   ;; Accessors for generic function metaobjects.
   #:generic-function-methods
   #:gf-methods
   #:generic-function-method-combination
   #:specializer-profile
   #:call-history
   ;; Accessors for method metaobjects.
   #:method-generic-function
   #:m-generic-function
   #:method-lambda-list
   #:method-specializers
   #:method-function
   #:accessor-method-slot-definition
   ;; Accessors for slot definition metaobjects.
   #:slot-definition-name
   #:slot-definition-location
   ;; Miscellaneous functions.
   #:heap-instance-p
   #:standard-instance-access)
  (:shadow #:defclass
	   #:ensure-generic-function
	   #:defgeneric
	   #:defmethod
	   #:add-method
	   #:remove-method
	   #:compute-applicable-methods))
