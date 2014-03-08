(cl:in-package #:common-lisp-user)

(defpackage #:sicl-boot-phase2
  (:use #:common-lisp #:aspiring-sicl-clos)
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
   ;; Types
   #:heap-instance
   ;; Miscellaneous functions.
   #:allocate-heap-instance
   #:allocate-slot-storage
   #:heap-instance-p
   #:heap-instance-class
   #:heap-instance-slots
   #:slot-contents
   #:standard-instance-access
   #:ensure-class
   #:ensure-built-in-class
   #:ensure-method
   #:find-bridge-class
   #:initialize-built-in-instance-default
   ;; Variables
   *unbound-value*)
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
   #:finalize-target-classes
   #:patch-target-objects
   #:make-instance-default
   #:slot-value-using-class
   #:slot-boundp-using-class
   #:allocate-instance-default
   #:allocate-built-in-instance
   #:find-target-class
   #:make-instance
   #:*make-instance-default*
   #:*shared-initialize-default*))
