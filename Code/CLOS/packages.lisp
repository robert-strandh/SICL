(defpackage #:sicl-clos
  (:shadow
   #:class #:standard-class #:built-in-class #:structure-class
   #:defclass #:find-class
   #:class-name #:class-of
   #:allocate-instance #:make-instance
   #:initialize-instance #:shared-initialize #:reinitialize-instance
   #:make-instances-obsolete
   #:update-instance-for-different-class
   #:update-instance-for-redefined-class
   #:method #:standard-method
   #:make-method #:add-method #:call-method #:find-method #:call-next-method
   #:method-qualifiers #:compute-applicable-methods #:next-method-p
   #:invalid-method-error
   #:no-applicable-method #:no-next-method #:remove-method #:defmethod
   #:method-combination #:define-method-combination #:method-combination-error
   #:make-instance
   #:generic-function #:standard-generic-function
   #:defgeneric #:ensure-generic-function
   #:slot-value #:slot-missing #:slot-boundp #:slot-makunbound
   #:print-object #:describe-object)
  (:use #:common-lisp))