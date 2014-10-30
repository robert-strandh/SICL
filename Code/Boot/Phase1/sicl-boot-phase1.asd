(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot-phase1
  :depends-on (:cleavir-code-utilities
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
   ;; Define generic functions DIRECT/EFFECTIVE-SLOT-DEFINITION-CLASS.
   ;;
   ;; The function DIRECT-SLOT-DEFINITION-CLASS is called by the
   ;; :AROUND method on INITIALIZE-INSTANCE specialized to certain
   ;; class metaobjects in order to convert canonicalized slot
   ;; descriptions to instances of whatever direct slot definition
   ;; class is wanted by that particular class.  The default method on
   ;; DIRECT-SLOT-DEFINITION-CLASS returns the value of the variable
   ;; *STANDARD-DIRECT-SLOT-DEFINITION*.  Since this function is
   ;; called when a bridge class (which is a host instance) is created
   ;; in order to create direct slot definition metaobjectss on that
   ;; class, those slot definition metaobjects will be host instances
   ;; as well.  This is why the value of the variable
   ;; *STANDARD-DIRECT-SLOT-DEFINITION* is set to the host class named
   ;; STANDARD-DIRECT-SLOT-DEFINITION.
   ;;
   ;; The function EFFECTIVE-SLOT-DEFINITION-CLASS is called by the
   ;; function COMPUTE-EFFECTIVE-SLOT-DEFINITION which is part of the
   ;; class finalization protocol in order to create an effective slot
   ;; definition metaobject from a list of direct slot definition
   ;; metaobjects.  Here, the effective slot definition metaobject we
   ;; want to create is a host instance, because it will be part of a
   ;; bridge class, and a bridge class is a host instance as well.
   ;; This is why the value of the variable
   ;; *STANDARD-EFFECTIVE-SLOT-DEFINITION* is set to the host class
   ;; named STANDARD-EFFECTIVE-SLOT-DEFINITION.
   (:file "slot-definition-class-support")
   (:file "slot-definition-class-defgenerics")
   (:file "slot-definition-class-defmethods")
   ;; Define a version of VALIDATE-SUPERCLASS as an ordinary function
   ;; that always returns true.  This function is called by the
   ;; :AROUND method on INITIALIZE-INSTANCE specialized to certain
   ;; class metaobjects as part of the class initialization protocol.
   ;; During bootstrapping, we do not expect any problems so we can
   ;; always return true.
   (:file "validate-superclass")
   ;; Define :AROUND and :AFTER methods on INITIALIZE-INSTANCE
   ;; specialized to certain class metaobjects, for the purpose of
   ;; implementing the class initialization protocol.  Here, these
   ;; methods will be on host generic functions in order to initialize
   ;; bridge classes.  
   (:file "class-initialization-support")
   (:file "class-initialization-defmethods")
   ;; Define host generic functions COMPUTE-APPLICABLE-METHODS and
   ;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES and default methods on
   ;; those generic functions.  Here they will be host generic
   ;; functions and host methods that take bridge generic functions as
   ;; arguments, and they will return lists of bridge methods.  These
   ;; functions are called by COMPUTE-DISCRIMINATING-FUNCTION. 
   (:file "compute-applicable-methods-support")
   (:file "compute-applicable-methods-defgenerics")
   (:file "compute-applicable-methods-defmethods")
   ;; Define host generic function COMPUTE-EFFECTIVE-METHOD.  The
   ;; effective method is computed from a sorted list of applicable
   ;; bridge methods by first computing a form containing the
   ;; applicable methods as literals, and then calling the compiler to
   ;; turn all that into an ordinary host function.  This function is
   ;; called by COMPUTE-DISCRIMINATING-FUNCTION.
   (:file "compute-effective-method-support")
   (:file "compute-effective-method-support-a")
   (:file "method-combination-compute-effective-method-support")
   (:file "method-combination-compute-effective-method-defgenerics")
   (:file "method-combination-compute-effective-method-defmethods")
   (:file "compute-effective-method-defgenerics")
   (:file "compute-effective-method-defmethods")
   ;; Define ordinary functions that do some of what the CL sequence
   ;; functions do, but that work only on lists.  We use these
   ;; functions to avoid using the sequence functions because we might
   ;; want to make the sequence functions generic, and we do not want
   ;; to invoke generic functions in order to compute the
   ;; discriminating function of generic functions.
   (:file "list-utilities")
   ;; Define ordinary functions for computing and minimizing the
   ;; discriminating automaton from the call history of a generic
   ;; function.  These functions are called by
   ;; COMPUTE-DISCRIMINATING-FUNCTION.
   (:file "discriminating-automaton")
   ;; Define ordinary functions for turning a discriminating automaton
   ;; into a discriminating tagbody form.  The tagbody form contains
   ;; effective methods as literals and is later compiled (using
   ;; COMPILE) into an ordinary host function.
   (:file "discriminating-tagbody")
   ;; Creating and initializing an ersatz instance requires querying
   ;; the class of that instance to obtain information about instance
   ;; size, slot initargs, etc.  But the class of an ersatz instance
   ;; is a bridge class, and a bridge class is a host instance.  So in
   ;; order to query the class of an ersatz instance, we need to use
   ;; the host generic functions defined here in phase 1.  This is why
   ;; we define what an ersatz instance is here, and also
   ;; ALLOCATE-INSTANCE and the support code for SHARED-INITIALIZE
   ;; (see below).
   (:file "ersatz-instance")
   ;; Define a somewhat strange version of CLASS-OF.  It is strange,
   ;; because it will sometimes be used on an ersatz instance and
   ;; sometimes on a host instance (bridge class, bridge generic
   ;; function, etc).  For that reason, we design it to work with
   ;; both.  
   (:file "class-of")
   ;; Define the specified ordinary function STANDARD-INSTANCE-ACCESS
   ;; and the unspecified function (SETF STANDARD-INSTANCE-ACCESS) to
   ;; work on ersatz instances.  These functions are used by the
   ;; support code of SHARED-INITIALIZE to initialize the ersatz
   ;; instance.
   (:file "standard-instance-access")
   ;; Define host generic function COMPUTE-DISCRIMINATING-FUNCTION.
   ;; Here, it will be used to compute the discriminating function of
   ;; a bridge generic function.  The discriminating function will be
   ;; computed by calling COMPILE on a lambda expression consisting
   ;; mainly of the TAGBODY form returned by the function 
   ;; COMPUTE-DISCRIMINATING-TAGBODY defined above.
   (:file "compute-discriminating-function-support")
   (:file "compute-discriminating-function-support-a")
   (:file "compute-discriminating-function-defgenerics")
   (:file "compute-discriminating-function-defmethods")
   ;; Although we do not use the dependent maintenance facility, we
   ;; define the specified functions as ordinary functions that do
   ;; nothing, so that we can safely call them from other code.
   (:file "dependent-maintenance-support")
   (:file "dependent-maintenance-defuns")
   ;; Define ordinary function SET-FUNCALLABLE-INSTANCE-FUNCTION to
   ;; set the funcallable instance function of a bridge generic
   ;; function.  Here in phase 1, a bridge generic function is a
   ;; subclass of the host class FUNCALLABLE-STANDARD-OBJECT, and a
   ;; host with a conforming CLOS implementation has its own version
   ;; of this function that works on instance is of the host class
   ;; FUNCALLABLE-STANDARD-OBJECT, so we just call the equivalent host
   ;; function.
   (:file "set-funcallable-instance-function")
   ;; Define :AROUND and :AFTER methods on the host generic functions
   ;; INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE in order to
   ;; implement the generic function initialization protocol so that
   ;; bridge generic functions can be properly initialized.
   (:file "generic-function-initialization-support")
   (:file "generic-function-initialization-defmethods")
   ;; Define host generic function DIRECT-SLOT-DEFINITION-P and
   ;; methods so that it returns true for a direct slot definition
   ;; metaobject. 
   (:file "direct-slot-definition-p")
   ;; Define :AROUND and :AFTER methods on the host generic function
   ;; INITIALIZE-INSTANCE in order to implement the method
   ;; initialization protocol so that bridge methods can be properly
   ;; initialized.
   (:file "method-initialization-support")
   (:file "method-initialization-defmethods")
   ;; Define primary methods on the host generic function PRINT-OBJECT
   ;; so that some host instances representing classes and slot
   ;; definition metaobjects are printed in a more convenient way.
   (:file "print-object")
   ;; Define host generic functions and methods for
   ;; FINALIZE-INHERITANCE, COMPUTE-CLASS-PRECEDENCE-LIST,
   ;; COMPUTE-DEFAULT-INITARGS, and COMPUTE-SLOTS in order to
   ;; implement class finalization for bridge classes.  These
   ;; functions must call accessors on bridge classes, and those
   ;; accessors are host generic functions which is why these
   ;; functions are defined here.
   (:file "class-finalization-defgenerics")
   (:file "class-finalization-support")
   (:file "class-finalization-defmethods")
   ;; Define ordinary functions COMPUTE-BUILT-IN-SLOTS and
   ;; FINALIZE-BUILT-IN-INHERITANCE to finalize built-in bridge
   ;; classes.
   (:file "built-in-class-finalization")
   ;; Define an ordinary function FINALIZE-BRIDGE-CLASSES that calls
   ;; FINALIZE-INHERITANCE on all the classes in *BRIDGE-CLASSES*.
   ;; This function will be called in phase 2 once all the bridge
   ;; classes that we will need have been created.
   (:file "finalize-bridge-classes")
   ;; Define generic function ALLOCATE-INSTANCE for the purpose of
   ;; creating ersatz instances.  ALLOCATE-INSTANCE creates ersatz
   ;; instances by instantiating bridge classes, passed to it as
   ;; arguments, and bridge classes are host instances.  Therefore
   ;; ALLOCATE-INSTANCE can be a host generic function.
   (:file "allocate-instance-support")
   (:file "allocate-instance-defgenerics")
   (:file "allocate-instance-defmethods")
   ;; Define ordinary function ALLOCATE-BUILT-IN-INSTANCE for the
   ;; purpose of allocating ersatz instances of built-in bridge
   ;; classes.
   (:file "allocate-built-in-instance")
   ;; When the support code for SHARED-INITIALIZE is used to
   ;; initialize an ersatz instance, it uses SLOT-BOUNDP-USING-CLASS
   ;; and (SETF SLOT-VALUE-USING-CLASS) to accomplish its task.  These
   ;; two functions are passed a bridge class (which is a host
   ;; instance) and a host instance representing an effective slot
   ;; definition metaobject.  This is why these two functions are
   ;; defined here as host generic functions.
   (:file "slot-value-etc-support")
   (:file "slot-value-etc-defgenerics")
   (:file "slot-value-etc-defmethods")
   ;; The function SHARED-INITALIZE initializes an instance, but it
   ;; accomplishes its task by accessing class metaobjects and
   ;; effective slot definition metaobjects.  When an ersatz instance
   ;; is initialized, the class metaobject is a bridge class (which is
   ;; a host instance) and the effective slot definition metaobject is
   ;; also a host instance.  The accessors that take such metaobjects
   ;; as arguments are therefore host generic functions.  This is why
   ;; the support code for shared-initialize is defined here.  On the
   ;; other hand, SHARED-INITIALIZE takes the ersatz instance itself
   ;; as an argument, so if it is defined as a generic function, it
   ;; would have to be a bridge generic function, but if
   ;; SHARED-INITIALIZE were defined here, it would be a host generic
   ;; function.  This is the reason that only the support code is
   ;; defined here and not the generic function.
   (:file "shared-initialize-support")
   ;; When a built-in ersatz instance is initialized there is no
   ;; equivalent of shared-initialize, simply because built-in
   ;; instances can not be reinitialized, so there is no need for
   ;; shared-initialize.  Instead the ordinary function named
   ;; INITIALIZE-BUILT-IN-INSTANCE does roughly the same work for a
   ;; built-in ersatz instance that SHARED-INITIALIZE does for a
   ;; standard instance.  So for the same reason that the support code
   ;; for SHARED-UNITIZE is defined here, the support code for
   ;; INITIALIZE-BUILT-IN-INSTANCE is also defined here.
   (:file "initialize-built-in-instance-support")
   ;; Define ordinary functions for support code for MAKE-INSTANCE for
   ;; the purpose of creating ersatz instance.  This support code
   ;; directly calls ALLOCATE-INSTANCE, defined above.  It also calls
   ;; INITIALIZE-INSTANCE, but as a function passed as an argument
   ;; because here INITIALIZE-INSTANCE means initializing host
   ;; instances.
   (:file "make-instance-support")
   ;; Define an ordinary function for support code for
   ;; MAKE-BUILT-IN-INSTANCE.  It mirrors the support code for
   ;; MAKE-INSTANCE in that it directly calls
   ;; ALLOCATE-BUILT-IN-INSTANCE defined above, and
   ;; INITIALIZE-BUILT-IN-INSTANCE passed as an argument
   (:file "make-built-in-instance-support")
   ;; Define an ordinary function SATIATE-BRIDGE-GENERIC-FUNCTION that
   ;; calls SATIATE-GENERIC-FUNCTION on all the generic functions in
   ;; *BRIDGE-GENERIC-FUNCTIONS*.  This function will be called in
   ;; phase 2 once all the bridge generic functions that we will need
   ;; have been created.
   (:file "satiate-generic-functions")
   ;; Remove the package nickname SICL-CLOS from the package named
   ;; SICL-BOOT-PHASE1.
   (:file "rename-package-2")))
