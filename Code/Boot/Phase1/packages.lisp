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
	   #:defclass
	   #:add-method
	   #:remove-method
	   #:compute-applicable-methods
	   #:ensure-generic-function
	   #:allocate-instance
	   #:slot-missing
	   #:slot-unbound
	   #:class-of)
  (:export
   ;; Names of classes
   #:standard-class
   #:funcallable-standard-class
   #:built-in-class
   #:standard-generic-function
   #:standard-method
   #:standard-reader-method
   #:standard-writer-method
   #:eql-specializer
   ;; Accessors for specializer metaobjects.
   #:specializer-direct-methods
   #:s-direct-methods
   #:class-direct-subclasses
   #:class-precedence-list
   #:class-prototype
   #:effective-slots
   #:unique-number
   #:eql-specializer-object
   ;; Accessors for generic function metaobjects.
   #:generic-function-methods
   #:gf-methods
   #:generic-function-method-class
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
   #:heap-instance-class
   #:heap-instance-slots
   #:slot-contents
   #:standard-instance-access
   #:ensure-class
   #:ensure-built-in-class
   #:ensure-generic-function
   #:ensure-method
   #:finalize-bridge-classes
   #:find-bridge-class
   #:satiate-bridge-generic-functions
   #:make-instance-default
   #:make-built-in-instance-default
   #:shared-initialize-default
   #:built-in-initialize-default
   #:allocate-built-in-instance
   ;; Variables
   *unbound-value*))
