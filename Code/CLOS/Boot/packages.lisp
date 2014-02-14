(cl:in-package #:common-lisp-user)

(defpackage #:sicl-clos
  (:use #:common-lisp)
  (:import-from #:sicl-code-utilities
		#:proper-list-p
		#:parse-generic-function-lambda-list
		#:parse-ordinary-lambda-list
		#:parse-specialized-lambda-list
		#:required)
  (:import-from #:sicl-additional-conditions #:no-such-class-name)
  (:shadow
   #:add-method
   #:allocate-instance
   #:built-in-class
   #:call-method
   #:call-next-method
   #:change-class
   #:class
   #:class-name
   #:class-of
   #:compute-applicable-methods
   #:defclass
   #:defgeneric
   #:defmethod
   #:define-method-combination
   #:ensure-generic-function
   #:find-class
   #:find-method
   #:function
   #:generic-function 
   #:initialize-instance
   #:make-instance
   #:method 
   #:method-combination
   #:method-qualifiers
   #:next-method-p
   #:no-next-method
   #:no-applicable-method
   #:reinitialize-instance
   #:remove-method
   #:shared-initialize
   #:slot-boundp
   #:slot-exists-p
   #:slot-makunbound
   #:slot-missing
   #:slot-unbound
   #:slot-value
   #:standard-class
   #:standard-generic-function
   #:standard-method
   #:standard-object
   #:update-instance-for-different-class
   #:update-instance-for-redefined-class
   #:compile)
  
  (:export))
