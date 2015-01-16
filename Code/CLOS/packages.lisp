(cl:in-package #:common-lisp-user)

;;; FIXME: The list of exported symbols is not complete.

(defpackage #:sicl-clos
  (:use #:common-lisp)
  (:import-from #:cleavir-code-utilities
		#:proper-list-p
		#:parse-generic-function-lambda-list
		#:parse-ordinary-lambda-list
		#:parse-specialized-lambda-list
		#:required)
  (:import-from #:sicl-additional-conditions #:no-such-class-name)
  (:export
   ;; MOP classes
   #:class #:standard-class #:built-in-class #:structure-class
   #:standard-object #:function
   #:generic-function #:standard-generic-function
   #:method #:standard-method
   #:method-combination
   #:slot-definition
   #:direct-slot-definition
   #:effective-slot-definition
   #:standard--slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   ;; Accessors for class metaobjects. 
   #:class-name
   #:class-direct-superclasses
   #:class-direct-slots
   #:class-direct-default-initargs
   #:class-precedence-list
   #:class-slots
   #:class-default-initargs
   #:class-finalized-p
   #:class-prototype
   ;; Accessors for generic function metaobjects.
   #:generic-function-name
   #:generic-function-argument-precedence-order
   #:generic-function-lambda-list
   #:generic-function-declarations
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   ;; Accessors for slot definition metaobjects.
   #:slot-definition-name
   #:slot-definition-allocation
   #:slot-definition-type
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-location
   ;; Macros
   #:defclass #:defgeneric #:defmethod
   ;; Generic functions.
   #:compute-effective-slot-definition
   #:find-class
   #:class-of
   #:allocate-instance
   #:make-instance
   #:initialize-instance
   #:reinitialize-instance
   #:shared-initialize
   #:make-instances-obsolete
   #:update-instance-for-different-class
   #:update-instance-for-redefined-class
   #:make-method #:add-method #:call-method #:find-method
   #:method-qualifiers #:compute-applicable-methods #:next-method-p
   #:invalid-method-error
   #:no-applicable-method #:no-next-method #:remove-method #:defmethod
   #:method-combination #:define-method-combination #:method-combination-error
   #:make-instance
   #:ensure-generic-function #:ensure-generic-function-using-class
   #:slot-value #:slot-missing #:slot-boundp #:slot-makunbound
   #:print-object #:describe-object #:documentation
   #:make-method-lambda
   ;; SICL-specific classes
   #:real-class #:regular-class
   ;; SICL-specific functions
   #:ensure-method
   ;; SICL-specific macro
   #:define-built-in-class))
