(cl:in-package #:common-lisp-user)

(defpackage #:aspiring-sicl-clos
  (:export
   ;; Supply the symbols that are names of MOP classes (including
   ;; SICL-specific classes) that are not supplied by the COMMON-LISP
   ;; package.
   #:funcallable-standard-object
   #:metaobject
   #:standard-accessor-method
   #:standard-reader-method
   #:standard-writer-method
   #:slot-definition
   #:direct-slot-definition
   #:effective-slot-definition
   #:standard-slot-definition
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:specializer
   #:eql-specializer
   #:funcallable-standard-class
   #:regular-class
   #:real-class
   #:forward-referenced-class))

(defpackage #:sicl-boot-common
  (:export
   #:*unbound-value*
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
   ))

(defpackage #:sicl-boot-phase1
  (:use #:common-lisp #:aspiring-sicl-clos #:sicl-boot-common)
  (:import-from #:cleavir-code-utilities
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
   #:initialize-built-in-instance-default
   #:class-of
   ;; Variables
   #:*more-names*))

;;; Names of symbols that should ultimately be exported from the
;;; SICL-CLOS package, exclusing names of symbols that are already
;;; exported from the COMMON-LISP package.
(defparameter sicl-boot-phase1:*more-names*
  (mapcar
   #'cl:symbol-name
   '(;; Accessors for class metaobjects. 
     #:class-direct-superclasses
     #:class-direct-slots
     #:class-direct-default-initargs
     #:class-precedence-list
     #:class-slots
     #:effective-slots
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
     ;; Accessors for method metaobjects.
     #:method-function
     #:method-generic-function
     #:method-lambda-list
     #:method-specializers
     #:accessor-method-slot-definition
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
     ;; Miscellaneous functions
     #:make-built-in-instance)))
