(cl:in-package #:common-lisp-user)

;;; FIXME: The list of exported symbols is not complete.

(defpackage #:sicl-clos
  (:use #:common-lisp)
  (:shadow #:documentation)
  (:export
   ;; Constants
   #:+unbound-slot-value+
   ;; MOP classes
   #:class #:standard-class #:built-in-class #:structure-class
   #:forward-referenced-class
   #:funcallable-standard-class
   #:standard-object #:function #:funcallable-standard-object
   #:simple-function
   #:generic-function #:standard-generic-function
   #:method #:standard-method
   #:metaobject
   #:specializer
   #:eql-specializer
   #:method-combination
   #:slot-definition
   #:direct-slot-definition
   #:effective-slot-definition
   #:standard-slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-writer-method
   ;; Accessors for generial standard objects.
   #:hash-code
   ;; Accessors for method metaobjects.
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-qualifiers
   #:method-specializers
   #:accessor-method-slot-definition
   ;; Accessors for specializer metaobjects.
   #:specializer-direct-generic-functions
   #:specializer-direct-methods
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
   #:instance-size
   #:variant-signature
   ;; Accessors for generic function metaobjects.
   #:entry-point
   #:environment
   #:code-object
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
   ;; Macro support
   #:with-slots-expander
   #:with-accessors-expander
   #:defgeneric-expander
   #:defmethod-expander
   #:parse-defmethod #:canonicalize-specializers
   #:defclass-expander
   ;; Generic functions.
   #:class-direct-subclasses
   #:add-direct-subclass
   #:compute-effective-slot-definition
   #:compute-default-initargs
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
   #:compute-applicable-methods #:next-method-p
   #:compute-applicable-methods-using-classes
   #:compute-discriminating-function
   #:invalid-method-error
   #:no-applicable-method #:no-next-method #:remove-method #:defmethod
   #:method-combination #:define-method-combination #:method-combination-error
   #:make-instance
   #:ensure-generic-function #:ensure-generic-function-using-class
   #:ensure-class #:ensure-class-using-class
   #:ensure-method
   #:slot-value #:slot-missing #:slot-boundp #:slot-makunbound
   #:slot-boundp-using-class
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #:print-object #:describe-object #:documentation
   #:make-method-lambda
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:reader-method-class #:writer-method-class
   #:validate-superclass
   #:finalize-inheritance
   #:map-dependents
   #:update-dependent
   #:find-method-combination
   ;; Other functions
   #:set-funcallable-instance-function
   #:shared-initialize-around-real-class-default
   ;; SICL-specific classes
   #:real-class #:regular-class
   ;; SICL-specific functions
   #:default-superclasses
   #:allocate-general-instance
   #:add-direct-method
   #:remove-direct-method
   ;; SICL-specific macro
   #:subclassp
   ;; Condition types
   #:no-such-class-name
   #:malformed-documentation-option
   #:attempt-to-access-precedence-list-of-unfinalized-class
   #:slot-definition-argument-must-be-supplied
   #:unable-to-compute-class-precedence-list))
