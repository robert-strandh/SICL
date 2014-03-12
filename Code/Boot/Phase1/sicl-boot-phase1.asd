(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot-phase1
  :depends-on (:sicl-code-utilities
	       :sicl-additional-conditions)
  :serial t
  :components
  (;; Define packages ASPIRING-SICL-CLOS and SICL-BOOT-PHASE1.  The
   ;; package ASPIRING-SICL-CLOS supplies the symbols that are names
   ;; of MOP classes, other than the names of such classes that are
   ;; supplied by the COMMON-LISP package.  The package
   ;; SICL-BOOT-PHASE1 imports the symbols of ASPIRING-SICL-CLOS and
   ;; shadows the symbol of the COMMON-LISP package that name MOP
   ;; classes and MOP generic functions.
   (:file "packages")
   ;; Add SICL-CLOS as a nickname to the package SICL-BOOT-PHASE1.
   ;; The purpose of this manipulation is so that CLOS code that is
   ;; loaded will be defined in the package SICL-BOOT-PHASE1.  
   (:file "rename-package-1")
   ;; Define a fake version of the macro DEFINE-BUILT-IN-CLASS that
   ;; expands to DEFCLASS, except that if the built-in class to be
   ;; defined is named T, then the entire DEFINE-BUILT-IN-CLASS form
   ;; expands to NIL instead.
   (:file "define-built-in-class")
   ;; Define a version of the macro DEFCLASS that expands to
   ;; CL:DEFCLASS, except that if the symbol T is among the list of
   ;; superclasses, then it is removed. 
   (:file "defclass")
   ;; Define the MOP classes as host classes.  The names of those
   ;; classes will be in the package SICL-BOOT-PHASE1 when in the real
   ;; system it would be in the COMMON-LISP package, and in the
   ;; package ASPIRING-SICL-CLOS when in the real system it would be
   ;; in the SICL-CLOS package.  Defining the MOP classes also defines
   ;; generic functions corresponding to the accessors.  These generic
   ;; functions will be ordinary host generic functions.  The names of
   ;; those functions will be in the package SICL-BOOT-PHASE1.
   (:file "mop-class-hierarchy")
   ;; Define the class BRIDGE-GENERIC-FUNCTION as a subclass of the
   ;; host class FUNCALLABLE-STANDARD-OBJECT and of the newly created
   ;; class SICL-BOOT-PHASE1:STANDARD-GENERIC-FUNCTION.
   (:file "bridge-generic-function")
   ;; Various parts of CLOS use default objects, such as the class
   ;; named STANDARD-OBJECT when no explicit superclass is given to
   ;; DEFCLASS, or the class STANDARD-DIRECT-SLOT-DEFINITION as the
   ;; default class of direct slot definition objects.  In the SICL
   ;; implementation of CLOS, these default objects are the values of
   ;; special variables (with only a global value).  We must therefore
   ;; define these variables, or cheat and define them as symbol macros. 
   (:file "define-variables")
   ;; Define a special variable *BRIDGE-CLASSES* to hold a list of
   ;; bridge classes, and define functions to query and modify the
   ;; contents in various ways.
   (:file "class-database")
   ;; Define a special version of ENSURE-CLASS that checks whether
   ;; there is a bridge class with the name that it is given as an
   ;; argument.  If not, it calls CL:MAKE-INSTANCE to create an
   ;; instance of the host class of that name, and adds the new class
   ;; to the database.  In either case, a bridge class is returned.
   ;; If no metaclass is given, it instantiates the host class named
   ;; STANDARD-CLASS.
   (:file "ensure-class")
   ;; Define a special version of ENSURE-BUILT-IN-CLASS that does the
   ;; same thing as ENSURE-CLASS, except that it takes no METACLASS
   ;; argument and it always calls CL:MAKE-INSTANCE with the symbol
   ;; BUILT-IN-CLASS as the metaclass to instantiate.
   (:file "ensure-built-in-class")
   ;; We define a generic function called CLASSP with a default method
   ;; that returns FALSE and a method specialized to CLASS that
   ;; returns true.
   (:file "classp")
   ;; We define a generic function called SPECIALIZERP with a default
   ;; method that returns FALSE and a method specialized to
   ;; SPECIALIZER that returns true.
   (:file "specializerp")
   ;; Define a special variable *BRIDGE-GENERIC-FUNCTIONS* to hold a
   ;; list of all bridge generic functions that are created, and
   ;; define function to query and add new functions.  When a bridge
   ;; generic function is added to the database, the FDEFINITION is
   ;; assigned as well, so that the function can be called normally.
   ;; We still need the database though, because later on, we are
   ;; going to want to satiate all bridge generic functions.
   (:file "generic-function-database")
   ;; Define a special version of ENSURE-GENERIC-FUNCTION that checks
   ;; whether there is a bridge generic function with the name given
   ;; as argument, if not, it calls CL:MAKE-INSTANCE to create a
   ;; bridge generic function with that name, and adds the new generic
   ;; function to the database (implicitly setting its FDEFINITION).
   ;; In either case, a bridge generic function is returned. 
   (:file "ensure-generic-function")
   ;; Define a special version of ENSURE-METHOD that calls
   ;; CL:MAKE-INSTANCE to make a host instance of the class
   ;; SICL-BOOT-PHASE1:STANDARD-METHOD and then adds the newly created
   ;; method to the bridge generic function that was given to it as an
   ;; argument.
   (:file "ensure-method")
   ;; Define generic functions SICL-BOOT-PHASE1:READER-METHOD-CLASS
   ;; and SICL-BOOT-PHASE1:WRITER-METHOD-CLASS and methods on these
   ;; functions that return whatever *STANDARD-READER-METHOD* and
   ;; *STANDARD-WRITER-METHOD* stand for.  These functions will be
   ;; called from ADD-READER/WRITER-METHOD when the MOP class
   ;; hierarchy is used in phase 2 to generate bridge classes.
   ;; Therefore, the classes that they should return are host classes,
   ;; so that when bridge classes (which are host instances) and
   ;; bridge generic functions (which are also host instances) are
   ;; created, the methods that are created are bridge methods (which
   ;; are also host instances).  This is why the variables
   ;; *STANDARD-READER-METHOD* and *STANDARD-WRITER-METHOD* contain
   ;; host classes.
   (:file "reader-writer-method-class-support")
   (:file "reader-writer-method-class-defgenerics")
   (:file "reader-writer-method-class-defmethods")
   ;; Define generic functions SICL-BOOT-PHASE1:ADD-DIRECT-METHOD and
   ;; SICL-BOOT-PHASE1:REMOVE-DIRECT-METHOD.  Recall that
   ;; ADD-DIRECT-METHOD is called to add a reference to a method from
   ;; a specializer (typically a class) used in that method.  It will
   ;; be called from ADD-METHOD when the MOP class hierarchy is used
   ;; in phase 2 to create bridge classes, in order to add a reference
   ;; to a bridge method from a bridge class.
   (:file "add-remove-direct-method-support")
   (:file "add-remove-direct-method-defgenerics")
   (:file "add-remove-direct-method-defmethods")
   ;; Define generic functions SICL-BOOT-PHASE1:ADD-METHOD and
   ;; SICL-BOOT-PHASE1:REMOVE-METHOD.  The function ADD-METHOD will be
   ;; called from ADD-READER/WRITER-METHOD when the MOP class
   ;; hierarchy is used in phase 2 to generate bridge classes.
   (:file "add-remove-method-support")
   (:file "add-remove-method-defgenerics")
   (:file "add-remove-method-defmethods")
   ;; Define ordinary function ENSURE-ACCESSOR-FUNCTION to call
   ;; ENSURE-GENERIC-FUNCTION.  Recall that ENSURE-ACCESSOR-FUNCTION
   ;; is an indirection to ENSURE-GENERIC-FUNCTION called by
   ;; ADD-READER/WRITER-METHOD in order to ensure that the generic
   ;; function exists.  The reason for the indirection is that we want
   ;; it to mean something different in phase 2.  Here it means that a
   ;; bridge generic function should be created if it does not already
   ;; exist.
   (:file "ensure-accessor-function")
   ;; Define ordinary functions ADD-READER/WRITER-METHOD.  These
   ;; functions are not part of the specification, but represent
   ;; convenient abstractions.  They are called from the :AFTER
   ;; methods on INITIALIZE-INSTANCE specialized to certain class
   ;; metaobjects.  Here, the purpose is to add the reader and writer
   ;; methods for each slot with such accessors when a bridge class is
   ;; created. 
   (:file "add-accessor-method")
   (:file "slot-definition-class-support")
   (:file "slot-definition-class-defuns")
   (:file "validate-superclass")
   (:file "class-initialization-support")
   (:file "class-initialization-defmethods")
   (:file "compute-applicable-methods-support")
   (:file "compute-applicable-methods-defgenerics")
   (:file "compute-applicable-methods-defmethods")
   (:file "compute-effective-method-support")
   (:file "compute-effective-method-support-a")
   (:file "method-combination-compute-effective-method-support")
   (:file "method-combination-compute-effective-method-defgenerics")
   (:file "method-combination-compute-effective-method-defmethods")
   (:file "compute-effective-method-defgenerics")
   (:file "compute-effective-method-defmethods")
   (:file "list-utilities")
   (:file "discriminating-automaton")
   (:file "discriminating-tagbody")
   (:file "heap-instance")
   (:file "class-of")
   (:file "standard-instance-access")
   (:file "compute-discriminating-function-support")
   (:file "compute-discriminating-function-support-a")
   (:file "compute-discriminating-function-defgenerics")
   (:file "compute-discriminating-function-defmethods")
   ;; Although we do not use the dependent maintenance facility, we
   ;; define the specified functions as ordinary functions that do
   ;; nothing, so that we can safely call them from other code.
   (:file "dependent-maintenance-support")
   (:file "dependent-maintenance-defuns")
   (:file "set-funcallable-instance-function")
   (:file "generic-function-initialization-support")
   (:file "generic-function-initialization-defmethods")
   (:file "direct-slot-definition-p")
   (:file "method-initialization-support")
   (:file "method-initialization-defmethods")
   (:file "print-object")
   (:file "class-finalization-defgenerics")
   (:file "class-finalization-support")
   (:file "class-finalization-defmethods")
   (:file "built-in-class-finalization")
   (:file "finalize-bridge-classes")
   (:file "allocate-instance-support")
   ;; We can not use the generic version of allocate instance, because
   ;; if we define a generic function here, it will be a host generic
   ;; function, and the generic version of allocate instance would
   ;; have to be a bridge generic function.
   (:file "allocate-instance-defuns")
   (:file "allocate-built-in-instance")
   (:file "slot-value-etc-support")
   (:file "slot-value-etc-defgenerics")
   (:file "slot-value-etc-defmethods")
   (:file "shared-initialize-support")
   (:file "initialize-built-in-instance-support")
   (:file "make-instance-support")
   (:file "make-built-in-instance-support")
   (:file "satiate-generic-functions")
   (:file "rename-package-2")))
