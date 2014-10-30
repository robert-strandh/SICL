(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot-phase2
  :depends-on (:cleavir-code-utilities
	       :sicl-additional-conditions
	       :sicl-boot-phase1)
  :serial t
  :components
  (;; Define package SICL-BOOT-PHASE2 which uses the COMMON-LISP
   ;; package and the ASPIRING-SICL-CLOS package.
   (:file "packages")
   ;; Define the package SICL-GLOBAL-ENVIRONMENT.  This package uses
   ;; the COMMON-LISP package and exports the names of all the classes
   ;; and functions required to manipulate a global environment.
   (:file "environment-packages")
   ;; Import and shadow symbols DEFCLASS, DEFGENERIC, and DEFMACRO
   ;; from the SICL-BOOT-PHASE2 package to the
   ;; SICL-GLOBAL-ENVIRONMENT, so that code that is loaded in the
   ;; package SICL-GLOBAL-ENVIRONMENT will use the definition of those
   ;; symbols in the SICL-BOOT-PHASE2 package.
   (:file "import-to-environment")
   ;; Define the package SICL-PACKAGE.  This package uses the
   ;; COMMON-LISP package and exports the names of all the classes and
   ;; functions required to manipulate packages.
   (:file "package-packages")
   ;; Before loading the definition of the class PACKAGE, we need to
   ;; import the symbol DEFINE-BUILT-IN-CLASS into the package
   ;; SICL-PACKAGE.
   (:file "import-to-package")
   ;; Define the package SICL-SYMBOL.  This package uses the
   ;; COMMON-LISP package and exports the names of all the classes and
   ;; functions required to manipulate packages.
   (:file "symbol-packages")
   ;; Before loading the definition of the class SYMBOL, we need to
   ;; make sure the accessors SYMBOL-NAME and SYMBOL-PACKAGE are
   ;; shadowed in the package SICL-SYMBOL.  Otherwise we will redefine
   ;; the host functions with that name.  Also, we need to import the
   ;; symbol DEFINE-BUILT-IN-CLASS into the package SICL-SYMBOL.
   (:file "import-to-symbol")
   ;; Define the package SICL-FUNCTION.  This package uses the
   ;; COMMON-LISP package and exports the names of all the classes and
   ;; functions required to manipulate functions.
   (:file "function-package")
   ;; Before loading the definitions of the function-related classes,
   ;; we need to import the symbols DEFINE-BUILT-IN-CLASS and DEFCLASS
   ;; to the package SICL-FUNCTION.
   (:file "import-to-function")
   ;; Define the package SICL-ARRAY.  This package uses the
   ;; COMMON-LISP package and exports the name of the SEQUENCE class
   ;; and the names of all the sequence functions.
   (:file "sequence-packages")
   ;; Before loading the definition of the class SEQUENCE, we need to
   ;; import the symbol DEFINE-BUILT-IN-CLASS into the package
   ;; SICL-SEQUENCE.
   (:file "import-to-sequence")
   ;; Define the package SICL-ARRAY.  This package uses the
   ;; COMMON-LISP package and exports the names of all the classes and
   ;; functions required to manipulate arrays.
   (:file "array-packages")
   ;; Before loading the definition of the class ARRAY, we need to
   ;; make sure the accessor ARRAY-DIMENSIONS is shadowed in the
   ;; package SICL-ARRAY.  Otherwise we will redefine the host
   ;; functions with that name.  Also, we need to import the symbol
   ;; DEFINE-BUILT-IN-CLASS into the package SICL-ARRAY.
   (:file "import-to-array")
   ;; Add nickname SICL-CLOS to the SICL-BOOT-PHASE2 package so that
   ;; code that gets loaded in the SICL-CLOS package in reality gets
   ;; loaded into the SICL-BOOT-PHASE2 package. 
   (:file "rename-package-1")
   ;; Define an ordinary function named
   ;; SICL-BOOT-PHASE2:SHARED-INITIALIZE-DEFAULT and that calls the
   ;; function which is the value of the variable
   ;; *SHARED-INITIALIZE-DEFAULT*.  The initial value of this variable
   ;; is the function SICL-BOOT-PHASE1:SHARED-INITIALIZE-DEFAULT, so
   ;; that SHARED-INITIALIZE initializes an ersatz instance by
   ;; accessing the bridge class of that instance.  Later, when the
   ;; class of an ersatz instance is another ersatz instance, we
   ;; change the value of the variable to be a function that
   ;; initializes ersatz instances by accessing the ersatz class of
   ;; the ersatz instance.
   (:file "shared-initialize-default")
   (:file "initialize-built-in-instance-default")
   ;; Up until now, the package named ASPIRING-SICL-CLOS contained
   ;; only the names of the specified classes whose names were not
   ;; exported from the package COMMON-LISP.  Now, we add names of
   ;; specified accessors to the package ASPIRING-SICL-CLOS so that
   ;; when we define bridge generic functions, their names will be
   ;; symbols in the package ASPIRING-SICL-CLOS. 
   (:file "import")
   ;; Define ordinary functions that do some of what the CL sequence
   ;; functions do, but that work only on lists.  We use these
   ;; functions to avoid using the sequence functions because we might
   ;; want to make the sequence functions generic, and we do not want
   ;; to invoke generic functions in order to compute the
   ;; discriminating function of generic functions.
   (:file "list-utilities")
   ;; Declare an ordinary function called FUNCTIONP and that always
   ;; returns true.  This function is used for error checking, and we
   ;; do not expect any errors during bootstrapping, so we can wing
   ;; it.
   (:file "functionp")
   ;; Define ordinary functions to be used by the expansion code for
   ;; DEFCLASS.  These functions are responsible for checking the
   ;; syntax of the DEFCLASS forms, and for canonicalizing superclass
   ;; specifications, slot specifications, and class options.
   (:file "defclass-support")
   ;; Define the macro SICL-BOOT-PHASE2:DEFCLASS.  It expands to a
   ;; call to ENSURE-CLASS.  The symbol SICL-BOOT-PHASE1:ENSURE-CLASS
   ;; is imported from phase 1, which means that this DEFCLASS will
   ;; create a bridge class.
   (:file "defclass-defmacro")
   ;; Define the macro SICL-BOOT-PHASE2:DEFINE-BUILT-IN-CLASS.  It
   ;; expands to a call to ENSURE-BUILT-IN-CLASS.  The symbol
   ;; SICL-BOOT-PHASE1:ENSURE-BUILT-IN-CLASS is imported from phase 1,
   ;; which means that this macro will create a built-in bridge class.
   (:file "define-built-in-class-defmacro")
   ;; Define the macro SICL-BOOT-PHASE2:DEFGENERIC.  It expands to a
   ;; call to ENSURE-GENERIC-FUNCTION.  The symbol
   ;; SICL-BOOT-PHASE1:ENSURE-GENERIC-FUNCTION is imported from phase
   ;; 1, which means that this DEFGENERIC will create a bridge generic
   ;; function.
   (:file "defgeneric-defmacro")
   ;; Define ordinary functions to be used by the expansion code for
   ;; DEFMETHOD.  These functions are responsible for checking the
   ;; syntax of the DEFMETHOD forms, and for canonicalizing the list
   ;; of specializers.
   (:file "defmethod-support")
   ;; Define the macro SICL-BOOT-PHASE2:DEFMETHOD.  It expands to a
   ;; call to ENSURE-METHOD.  The symbol
   ;; SICL-BOOT-PHASE1:ENSURE-METHOD is imported from phase 1, which
   ;; means that this DEFMETHOD will create a bridge method.
   (:file "defmethod-defmacro")
   ;; Define MAKE-METHOD-LAMBDA as an ordinary function.
   ;; MAKE-METHOD-LAMBDA is called by the expansion code of DEFMETHOD
   ;; in order to turn the method body into a lambda expression
   ;; suitable for method invocation. 
   (:file "make-method-lambda-support")
   (:file "make-method-lambda-defuns")
   ;; Load the hierarchy of MOP classes, which will create bridge
   ;; classes, bridge generic functions, bridge methods, and bridge
   ;; slot definitions.  All of these are instances of host classes. 
   (:file "mop-class-hierarchy")
   ;; Here in phase 2 is the right place to define bridge classes for
   ;; which the accessors are going to be used later during the
   ;; bootstrapping process, because those accessors will be the
   ;; FDEFINITION of the corresponding symbols, so they will be usable
   ;; as ordinary functions in the host bootstrapping environment.  In
   ;; later phases, defining classes and generic functions will define
   ;; ersatz generic functions which are not executable in the host
   ;; environment.  Since we are going to want to load up a global
   ;; ersatz environment with ersatz classes and ersatz functions, it
   ;; is handy (though not totally necessary, because we could use
   ;; reinitialize-instance) to have executable accessors for that
   ;; global ersatz environment.  This is why we include the
   ;; definition of environment classes here.
   (:file "environment-classes")
   (:file "environment-constructors")
   (:file "environment-query")
   ;; Define built-in bridge class SYMBOL and its accessors.
   (:file "symbol-class")
   ;; Define built-in bridge class PACKAGE and its accessors.
   (:file "package-class")
   (:file "function-class")
   (:file "compilation-unit-class")
   ;; Define built-in bridge class SEQUENCE.
   (:file "sequence-class")
   ;; Define built-in bridge class ARRAY and its subclasses.
   (:file "array-classes")
   ;; Now that we have all the bridge classes (but not all the bridge
   ;; generic functions) that we are ever going to define, it is time
   ;; to think about how to use those bridge classes and those bridge
   ;; generic functions in order to create ersatz instances.  As in
   ;; phase 1, there are a number of default values that are needed
   ;; during this process, such as the class named STANDARD-OBJECT,
   ;; the class named STANDARD-READER-METHOD.  These default values
   ;; are accessed as values of special variables.  This component
   ;; assigns meaning to those variables.
   (:file "define-variables")
   ;; Since we have all the bridge classes that we are ever going to
   ;; define, we finalize them all now.  FIXME: This step may actually
   ;; not be necessary.
   (:file "finalize-all-bridge-classes")
   ;; While we already have all the bridge classes we need, we need
   ;; more bridge generic functions.  The additional ones we need are
   ;; not accessors, but instead the entire remaining MOP machinery.
   ;; We can not define that machinery in the form of generic
   ;; functions here, because part of the purpose of the machinery is
   ;; to make generic functions work.  For that reason, we define the
   ;; machinery in the form of ordinary functions.  To begin with, we
   ;; define ordinary functions SICL-BOOT-PHASE1:ADD-DIRECT-METHOD and
   ;; SICL-BOOT-PHASE1:REMOVE-DIRECT-METHOD.  Recall that
   ;; ADD-DIRECT-METHOD is called to add a reference to a method from
   ;; a specializer (typically a class) used in that method.  It will
   ;; be called from ADD-METHOD when the MOP class hierarchy is used
   ;; in phase 3 to create ersatz classes, in order to add a reference
   ;; to an ersatz method from an ersatz class.
   (:file "add-remove-direct-method-support")
   (:file "add-remove-direct-method-defuns")
   ;; Define an ordinary function CLASSP that always returns true.
   ;; The function CLASSP is used for the purpose of error checking,
   ;; and during bootstrapping, we do not expect any errors, so we can
   ;; wing it.
   (:file "classp")
   ;; Define ordinary functions COMPUTE-APPLICABLE-METHODS and
   ;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES.  Here they will be
   ;; ordinary functions that take ersatz generic functions as
   ;; arguments, and they will return lists of ersatz methods.  These
   ;; functions are called by COMPUTE-DISCRIMINATING-FUNCTION.
   (:file "compute-applicable-methods-support")
   (:file "compute-applicable-methods-defuns")
   ;; Define ordinary function COMPUTE-EFFECTIVE-METHOD.  The
   ;; effective method is computed from a sorted list of applicable
   ;; ersatz methods by first computing a form containing the
   ;; applicable methods as literals, and then calling the cross
   ;; compiler to turn all that into an ersatz function.  This
   ;; function is called by COMPUTE-DISCRIMINATING-FUNCTION.
   (:file "compute-effective-method-support")
   (:file "compute-effective-method-support-a")
   (:file "method-combination-compute-effective-method-support")
   (:file "method-combination-compute-effective-method-defuns")
   (:file "compute-effective-method-defuns")
   ;; Define ordinary functions for computing and minimizing the
   ;; discriminating automaton from the call history of a generic
   ;; function.  These functions are called by
   ;; COMPUTE-DISCRIMINATING-FUNCTION.
   (:file "discriminating-automaton")
   ;; Define ordinary functions for turning a discriminating automaton
   ;; into a discriminating tagbody form.  The tagbody form contains
   ;; effective methods as literals and is later compiled (using
   ;; the cross compiler) into an ersatz function.
   (:file "discriminating-tagbody")
   ;; Define function COMPILE to mean the cross compiler which creates
   ;; an ersatz function from a lambda expression.
   (:file "compile")
   ;; Define ordinary function COMPUTE-DISCRIMINATING-FUNCTION.  Here,
   ;; it will be used to compute the discriminating function of an
   ;; ersatz generic function.  The discriminating function will be
   ;; computed by calling the cross compiler on a lambda expression
   ;; consisting mainly of the TAGBODY form returned by the function
   ;; COMPUTE-DISCRIMINATING-TAGBODY defined above.
   (:file "compute-discriminating-function-support")
   (:file "compute-discriminating-function-support-a")
   (:file "compute-discriminating-function-defuns")
   ;; Although we do not use the dependent maintenance facility, we
   ;; define the specified functions as ordinary functions that do
   ;; nothing, so that we can safely call them from other code.
   (:file "dependent-maintenance-support")
   (:file "dependent-maintenance-defuns")
   ;; Define ordinary function SET-FUNCALLABLE-INSTANCE-FUNCTION to
   ;; set the funcallable instance function of an ersatz generic
   ;; function.  FIXME: right now, this function does nothing.  What
   ;; it should do is to copy the slots of the discriminating function
   ;; (which is an ordinary ersatz function) to the generic function
   ;; (which is an ersatz generic function). 
   (:file "set-funcallable-instance-function")
   ;; Define bridge generic functions SICL-BOOT-PHASE2:ADD-METHOD and
   ;; SICL-BOOT-PHASE2:REMOVE-METHOD.  The function ADD-METHOD will be
   ;; called from ADD-READER/WRITER-METHOD when the MOP class
   ;; hierarchy is used in phase 3 to generate ersatz classes.
   (:file "add-remove-method-support")
   (:file "add-remove-method-defgenerics")
   (:file "add-remove-method-defmethods")
   ;; When the support code for SHARED-INITIALIZE is used to
   ;; initialize an ersatz instance, it uses SLOT-BOUNDP-USING-CLASS
   ;; and (SETF SLOT-VALUE-USING-CLASS) to accomplish its task.  Here,
   ;; these two functions are passed an ersatz class (which is a host
   ;; instance) and an ersatz instance representing an effective slot
   ;; definition metaobject.  This is why these two functions are
   ;; defined here as bridge generic functions.
   (:file "slot-value-etc-defgenerics")
   (:file "slot-value-etc-support")
   (:file "slot-value-etc-defmethods")
   (:file "slot-value-etc-specified-defuns")
   ;; Define bridge generic functions INITIALIZE-INSTANCE,
   ;; REINITIALIZE-INSTANCE, and shared-initialize.
   (:file "initialize-instance-defgenerics")
   (:file "reinitialize-instance-defgenerics")
   (:file "shared-initialize-defgenerics")
   ;; Define ordinary functions constituting support code for
   ;; INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE.  The function
   ;; SHARED-INITIALIZE-DEFAULT has already been defined before in
   ;; phase 2 to make an indirect call to the version of
   ;; SHARED-INITIALIZE-DEFAULT that we defined in phase 1, so we do
   ;; not include it here.
   (:file "initialize-instance-support")
   (:file "reinitialize-instance-support")
   ;; Define bridge methods on INITIALIZE-INSTANCE,
   ;; REINITIALIZE-INSTANCE, and shared-initialize.
   (:file "initialize-instance-defmethods")
   (:file "reinitialize-instance-defmethods")   
   (:file "shared-initialize-defmethods")
   ;; Define INITIALIZE-BUILT-IN-INSTANCE as a bridge generic
   ;; function, and default method.
   (:file "initialize-built-in-instance-defgenerics")
   (:file "initialize-built-in-instance-defmethods")
   ;; Define an ordinary function SPECIALIZERP that always returns
   ;; true.  This function is used only for error checking, and during
   ;; bootstrapping, we do not expect this kind of error.
   (:file "specializerp")
   ;; Define bridge generic functions
   ;; SICL-BOOT-PHASE2:READER-METHOD-CLASS and
   ;; SICL-BOOT-PHASE2:WRITER-METHOD-CLASS and methods on these
   ;; functions that return whatever *STANDARD-READER-METHOD* and
   ;; *STANDARD-WRITER-METHOD* stand for.  These functions will be
   ;; called from ADD-READER/WRITER-METHOD when the MOP class
   ;; hierarchy is used in phase 3 to generate ersatz classes by
   ;; instantiating bridge classes.  Therefore, the classes that they
   ;; should return are bridge classes, so that when ersatz classes
   ;; and ersatz generic functions are created by instantiating bridge
   ;; classes, the methods that are created are ersatz methods.  This
   ;; is why the variables *STANDARD-READER-METHOD* and
   ;; *STANDARD-WRITER-METHOD* contain bridge classes, as can be seen
   ;; in the component define-variables.lisp
   (:file "reader-writer-method-class-support")
   (:file "reader-writer-method-class-defgenerics")
   (:file "reader-writer-method-class-defmethods")
   ;; Define an ordinary function named SICL-BOOT-PHASE2:MAKE-INSTANCE
   ;; and that calls the function which is the value of the variable
   ;; *MAKE-INSTANCE-DEFAULT*.  The initial value of this variable is
   ;; the function SICL-BOOT-PHASE1:MAKE-INSTANCE-DEFAULT, so that
   ;; MAKE-INSTANCE creates an ersatz instance by instantiating a
   ;; bridge class.  Later, when we have ersatz classes, we change the
   ;; value of the variable to be a function that creates an ersatz
   ;; instance by instantiating an ersatz class.
   (:file "make-instance")
   ;; Define ordinary function MAKE-BUILT-IN-INSTANCE that calls
   ;; SICL-BOOT-PHASE1:MAKE-BUILT-IN-INSTANCE-DEFAULT, thus making a
   ;; built-in instance by instantiating a built-in bridge class.
   (:file "make-built-in-instance")
   ;; Define ordinary function ENSURE-ACCESSOR-FUNCTION to call
   ;; *ENSURE-GENERIC-FUNCTION.  Recall that ENSURE-ACCESSOR-FUNCTION
   ;; is an indirection to ENSURE-GENERIC-FUNCTION called by
   ;; ADD-READER/WRITER-METHOD in order to ensure that the generic
   ;; function exists.  The reason for the indirection is that we want
   ;; it to mean something different in phase 1.  Here it means that
   ;; an ersatz generic function should be created if it does not
   ;; already exist.
   (:file "ensure-accessor-function")
   ;; Define ordinary functions ADD-READER/WRITER-METHOD.  These
   ;; functions are not part of the specification, but represent
   ;; convenient abstractions.  They are called from the :AFTER
   ;; methods on INITIALIZE-INSTANCE specialized to certain class
   ;; metaobjects.  Here, the purpose is to add the reader and writer
   ;; methods for each slot with such accessors when an ersatz class
   ;; is created.
   (:file "add-accessor-method")
   ;; Define bridge generic functions
   ;; DIRECT/EFFECTIVE-SLOT-DEFINITION-CLASS.
   ;;
   ;; The function DIRECT-SLOT-DEFINITION-CLASS is called by the
   ;; :AROUND method on INITIALIZE-INSTANCE specialized to certain
   ;; class metaobjects in order to convert canonicalized slot
   ;; descriptions to instances of whatever direct slot definition
   ;; class is wanted by that particular class.  The default method on
   ;; DIRECT-SLOT-DEFINITION-CLASS returns the value of the variable
   ;; *STANDARD-DIRECT-SLOT-DEFINITION*.  Since this function is
   ;; called when an ersatz class is created in order to create direct
   ;; slot definition metaobjectss on that class, those slot
   ;; definition metaobjects will be ersatz instances as well.  This
   ;; is why the value of the variable
   ;; *STANDARD-DIRECT-SLOT-DEFINITION* is set to the bridge class
   ;; named STANDARD-DIRECT-SLOT-DEFINITION.
   ;;
   ;; The function EFFECTIVE-SLOT-DEFINITION-CLASS is called by the
   ;; function COMPUTE-EFFECTIVE-SLOT-DEFINITION which is part of the
   ;; class finalization protocol in order to create an effective slot
   ;; definition metaobject from a list of direct slot definition
   ;; metaobjects.  Here, the effective slot definition metaobject we
   ;; want to create is a an ersatz instance, because it will be part
   ;; of an ersatz class.  This is why the value of the variable
   ;; *STANDARD-EFFECTIVE-SLOT-DEFINITION* is set to the bridge class
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
   ;; methods will be on bridge generic functions in order to initialize
   ;; ersatz classes.  
   (:file "class-initialization-support")
   (:file "class-initialization-defmethods")
   ;; Define :AROUND and :AFTER methods on the bridge generic
   ;; functions INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE in order
   ;; to implement the generic function initialization protocol so
   ;; that ersatz generic functions can be properly initialized.
   (:file "generic-function-initialization-support")
   (:file "generic-function-initialization-defmethods")
   ;; Define bridge generic function direct-slot-definition-p with
   ;; methods so that it returns true for a slot definition
   ;; metaobject, and false otherwise.
   (:file "direct-slot-definition-p")
   ;; Define :AROUND and :AFTER methods on the bridge generic function
   ;; INITIALIZE-INSTANCE in order to implement the method
   ;; initialization protocol so that ersatz methods can be properly
   ;; initialized.
   (:file "method-initialization-support")
   (:file "method-initialization-defmethods")
   ;; Define a special variable *ERSATZ-CLASSES* to hold a list of
   ;; ersatz classes, and define functions to query and modify the
   ;; contents in various ways.
   (:file "class-database")
   ;; Define a special variable *ERSATZ-GENERIC-FUNCTIONS* to hold a
   ;; list of all ersatz generic functions that are created, and
   ;; define function to query and add new functions.
   (:file "generic-function-database")
   ;; Define bridge generic functions and methods for
   ;; FINALIZE-INHERITANCE, COMPUTE-CLASS-PRECEDENCE-LIST,
   ;; COMPUTE-DEFAULT-INITARGS, and COMPUTE-SLOTS in order to
   ;; implement class finalization for ersatz classes.  These
   ;; functions must call accessors on ersatz classes, and those
   ;; accessors are bridge generic functions which is why these
   ;; functions are defined here.
   (:file "class-finalization-defgenerics")
   (:file "class-finalization-support")
   (:file "class-finalization-defmethods")
   ;; Define ordinary functions COMPUTE-BUILT-IN-SLOTS and
   ;; FINALIZE-BUILT-IN-INHERITANCE to finalize built-in ersatz
   ;; classes.
   (:file "built-in-class-finalization")
   ;; Define ordinary function FINALIZE-ERSATZ-CLASSES that finalizes
   ;; all the classes in *ERSATZ-CLASSES*.  This function will be
   ;; called in phase 3 once all the ersatz classes have been created.
   (:file "finalize-ersatz-classes")
   ;; Define ordinary functions, generic functions, and methods for
   ;; patching ersatz instances.  Patching an ersatz instance means
   ;; replacing host instances (such as bridge classes) by ersatz
   ;; instances.  We patch classes and slot definition metaobjects,
   ;; but we do not patch initfunctions, because we need for
   ;; initfunctions to be executable in the host environment later
   ;; when we make ersatz instances from ersatz classes.
   (:file "patch-ersatz-objects")
   ;; Define bridge generic function PRINT-OBJECT and methods specialized to
   ;; all metaobject classes. 
   (:file "print-object")
   ;; At this point, all specified bridge generic functions should
   ;; have been defined, because the next step is to satiate them all
   ;; by calling the entry in phase1 for doing so.  Furthermore, at
   ;; this point, every class that we ever want make instances of must
   ;; have been defined.  The reason is that we satiate the bridge
   ;; generic functions with respect to classes that have been defined
   ;; so far, and we count on generic functions to be satiated.  If a
   ;; bridge generic function is ever called with an ersatz instance
   ;; whose class is an ersatz class, and which requires the general
   ;; machinery of a generic function, we are in trouble.  The reason
   ;; is that the specilizers of bridge methods are bridge classes, so
   ;; we would then try to compare an ersatz class and a bridge class,
   ;; which will fail.
   (:file "satiate-all-generic-functions")
   ;; Here in phase 2, ENSURE-CLASS, ENSURE-BUILT-IN-CLASS,
   ;; ENSURE-GENERIC-FUNCTION, and ENSURE-METHOD are symbols that are
   ;; imported from the package SICL-BOOT-PHASE1 and they mean that a
   ;; bridge class, a bridge generic function, or a bridge method
   ;; should be created from a host class.  In phase 3, we need for
   ;; these names to mean that an ersatz instance should be created
   ;; from a bridge class.  However, the computations to obtain that
   ;; effect belong here in phase 2.  For that reason, we define
   ;; ordinary functions *ENSURE-CLASS, *ENSURE-BUILT-IN-CLASS,
   ;; *ENSURE-GENERIC-FUNCTION, and *ENSURE-METHOD here, and that are
   ;; called from the analogous ENSURE-... functions in phase 3.
   (:file "xensure-class")
   (:file "xensure-built-in-class")
   (:file "xensure-generic-function")
   (:file "xensure-method")
   ;; Define ordinary function ALLOCATE-INSTANCE that allocates an
   ;; ersatz instance from a bridge class.  This function is called
   ;; from the support code for MAKE-INSTANCE defined below.
   (:file "allocate-instance-support")
   (:file "allocate-instance-defuns")
   ;; Define ordinary function ALLOCATE-BUILT-IN-INSTANCE that
   ;; allocates a built-in ersatz instance from a bridge class.  This
   ;; function is called from the support code for
   ;; MAKE-BUILT-IN-INSTANCE in phase 3.
   (:file "allocate-built-in-instance")
   (:file "make-instance-support")
   (:file "make-built-in-instance-support")
   (:file "rename-package-2")))
