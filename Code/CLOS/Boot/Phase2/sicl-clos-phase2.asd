(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-clos-phase2
  :depends-on (:sicl-clos-phase1)
  ;; We use :SERIAL T so as to reduce the clutter with respect to the
  ;; dependencies, and to make the order completely predictable.
  :serial t
  :components
  (;; The symbol MAKE-INSTANCE is shadowed in the SICL-CLOS package,
   ;; but now, we need for MAKE-INSTANCE to do the same thing as
   ;; CL:MAKE-INSTANCE, so we define SICL-CLOS as a function that just
   ;; calls CL:MAKE-INSTANCE.
   (:file "make-instance")
   ;; The symbol COMPILE is shadowed in the SICL-CLOS package, but
   ;; now, we need for COMPILE to do the same thing as CL:COMPILE, so
   ;; we define SICL-CLOS as a function that just calls CL:COMPILE.
   (:file "compile")
   ;; The symbols INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and
   ;; SHARED-INITIALIZED are shadowed in the SICL-CLOS package.  But
   ;; we still need for the :AFTER methods on INITIALIZE-INSTANCE
   ;; defined here to be called as :AFTER method on
   ;; CL:INITIALIZE-INSTANCE.  We solve this problem by 1: Defining
   ;; SICL-CLOS:INITIALIZE-INSTANCE as a host generic function (this
   ;; happens when we load "initialize-defgenerics"), 2: Defining a
   ;; primary unspecialized method on SICL-CLOS:INITIALIZE-INSTANCE
   ;; that does nothing (so that there is always a primary applicable
   ;; method), and 3: Defining an :AFTER method on
   ;; CL:INITIALIZE-INSTANCE specialized to SICL-CLOS:METAOBJECT that
   ;; calls SICL-CLOS:INITIALIZE-INSTANCE (this is done in
   ;; "initialize-instance")
   (:file "initialize-defgenerics")
   (:file "initialize-instance")
   ;; Although we do not use the dependent maintenance facility, we
   ;; define the specified functions as ordinary functions that do
   ;; nothing, so that we can safely call them from other code.
   (:file "dependent-maintenance-support")
   (:file "dependent-maintenance-defuns")
   ;; The symbol FIND-CLASS is shadowed in the SICL-CLOS package, but
   ;; now, we need for FIND-CLASS to do the same thing as
   ;; CL:FIND-CLASS, so we define SICL-CLOS as a function that just
   ;; calls CL:FIND-CLASS.
   (:file "find-class")
   ;; Define the specified ordinary function
   ;; SET-FUNCALLABLE-INSTANCE-FUNCTION to set the discriminating
   ;; function of the bridge generic function. 
   (:file "set-funcallable-instance-function")
   ;; Define :AFTER methods on INITIALIZE-INSTANCE that implement the
   ;; generic function initialization protocol
   (:file "generic-function-initialization-support")
   (:file "generic-function-initialization-defmethods")
   (:file "bridge-generic-function")
   (:file "generic-function-database")
   (:file "fmakunbound-defgeneric")
   (:file "defgeneric-defmacro")
   (:file "specializerp")
   (:file "method-initialization-support")
   (:file "method-initialization-defmethods")
   (:file "ensure-generic-function")
   (:file "slot-value")
   (:file "add-remove-direct-method-support")
   (:file "add-remove-direct-method-defuns")
   (:file "class-of")
   (:file "compute-applicable-methods-support")
   (:file "compute-applicable-methods-defuns")
   (:file "compute-effective-method-support")
   (:file "compute-effective-method-support-b")
   (:file "compute-effective-method-defuns")
   (:file "no-applicable-method-defuns")
   (:file "compute-discriminating-function-support")
   (:file "compute-discriminating-function-support-a")
   (:file "compute-discriminating-function-defuns")
   (:file "accessor-defgenerics")
   (:file "add-remove-method-support")
   (:file "add-remove-method-defuns")
   (:file "class-database")
   (:file "find-class-named-t")
   (:file "reader-writer-method-classes")
   (:file "reader-writer-method-class-support")
   (:file "reader-writer-method-class-defuns")
   (:file "make-accessor-method-a")
   (:file "add-accessor-method")
   (:file "slot-definition-classes")
   (:file "slot-definition-class-support")
   (:file "slot-definition-class-defuns")
   (:file "classp")
   (:file "standard-object-classes")
   (:file "validate-superclass")
   (:file "class-initialization-support")
   (:file "class-initialization-defmethods")
   (:file "fmakunbound-define-built-in-class")
   (:file "fmakunbound-defclass")
   (:file "class-finalization-support")
   (:file "class-finalization-defuns")
   (:file "built-in-class-finalization")
   (:file "ensure-built-in-class")
   (:file "ensure-class")
   (:file "defclass-support")
   (:file "defclass-defmacro")
   (:file "define-built-in-class-defmacro")
   (:file "make-method-lambda-support")
   (:file "make-method-lambda-defuns")
   (:file "defmethod-support")
   (:file "fmakunbound-defmethod")
   (:file "ensure-method")
   (:file "defmethod-defmacro")
   (:file "mop-class-hierarchy")
   (:file "additional-classes")
   ))
