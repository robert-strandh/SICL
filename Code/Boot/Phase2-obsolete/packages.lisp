(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase2
  (:use #:common-lisp #:aspiring-sicl-clos #:sicl-boot-common)
  (:import-from #:cleavir-code-utilities
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
   #:ensure-class
   #:ensure-built-in-class
   #:ensure-method
   #:find-bridge-class)
  (:shadow #:defclass
	   #:defgeneric
	   #:defmethod
	   #:add-method
	   #:remove-method
	   #:compute-applicable-methods
	   #:method-qualifiers
	   #:class-name
	   #:initialize-instance
	   #:reinitialize-instance
	   #:shared-initialize
	   #:slot-exists-p
	   #:slot-value
	   #:slot-boundp
	   #:slot-unbound
	   #:slot-makunbound
	   #:slot-missing
	   #:make-instance
	   #:compile
	   #:functionp
	   #:print-object
	   #:allocate-instance)
  (:export
   #:*ensure-class
   #:*ensure-built-in-class
   #:*ensure-generic-function
   #:*ensure-method
   #:finalize-ersatz-classes
   #:patch-ersatz-objects
   #:make-instance-default
   #:make-built-in-instance-default
   #:slot-value-using-class
   #:slot-boundp-using-class
   #:allocate-instance-default
   #:allocate-built-in-instance
   #:find-ersatz-class
   #:add-ersatz-class
   #:find-ersatz-generic-function
   #:add-ersatz-generic-function
   #:make-instance
   #:*make-instance-default*
   #:*make-built-in-instance-default*
   #:*shared-initialize-default*
   #:*initialize-built-in-instance-default*
   #:*find-class*
   #:*find-ersatz-class*
   #:*add-ersatz-class*
   #:*find-ersatz-generic-function*
   #:*add-ersatz-generic-function*))
