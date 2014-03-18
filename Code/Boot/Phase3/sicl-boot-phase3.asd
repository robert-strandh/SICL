(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-boot-phase3
  :depends-on (:sicl-boot-phase2)
  :serial t
  :components
  (;; Define package SICL-BOOT-PHASE3.  It uses the package named
   ;; COMMON-LISP and also the package named ASPIRING-SICL-CLOS.
   (:file "packages")
   ;; Add nickname SICL-CLOS to the package SICL-BOOT-PHASE3.
   (:file "rename-package-1")
   ;; Import name of accessors for MOP classes into the package named
   ;; ASPIRING-SICL-CLOS.  In phases 1 and 2, these names were symbols
   ;; in the BOOT packages.  Now, we want them to be in the
   ;; ASPIRING-SICL-CLOS package because we want the ersatz generic
   ;; functions to have the correct names.
   (:file "import")
   ;; Shadowing import the names of the specified macros DEFCLASS,
   ;; DEFGENERIC, and DEFMETHOD into the package
   ;; SICL-GLOBAL-ENVIRONMENT from the package SICL-BOOT-PHASE3, so
   ;; that when we load the class definitions for the global
   ;; environment, they will be defined as ersatz classes.
   (:file "import-to-environment")
   ;; Shadowing import the names of the macros DEFCLASS,
   ;; DEFINE-BUILT-IN-CLASS, DEFGENERIC, and DEFMETHOD into the
   ;; package SICL-PACKAGE from the package SICL-BOOT-PHASE3, so that
   ;; when we load the class definitions for the global environment,
   ;; they will be defined as ersatz classes.
   (:file "import-to-package")
   ;; Shadowing import the names of the macros DEFCLASS,
   ;; DEFINE-BUILT-IN-CLASS, DEFGENERIC, and DEFMETHOD into the
   ;; package SICL-SYMBOL from the package SICL-BOOT-PHASE3, so that
   ;; when we load the class definitions for the global environment,
   ;; they will be defined as ersatz classes.
   (:file "import-to-symbol")
   ;; Shadowing import the names of the macros DEFCLASS,
   ;; DEFINE-BUILT-IN-CLASS, DEFGENERIC, and DEFMETHOD into the
   ;; package SICL-SEQUENCE from the package SICL-BOOT-PHASE3, so that
   ;; when we load the class definitions for the global environment,
   ;; they will be defined as ersatz classes.
   (:file "import-to-sequence")
   ;; Shadowing import the names of the macros DEFCLASS,
   ;; DEFINE-BUILT-IN-CLASS, DEFGENERIC, and DEFMETHOD into the
   ;; package SICL-ARRAY from the package SICL-BOOT-PHASE3, so that
   ;; when we load the class definitions for the global environment,
   ;; they will be defined as ersatz classes.
   (:file "import-to-array")
   ;; Define ordinary functions that do some of what the CL sequence
   ;; functions do, but that work only on lists.  We use these
   ;; functions to avoid using the sequence functions because we might
   ;; want to make the sequence functions generic, and we do not want
   ;; to invoke generic functions in order to compute the
   ;; discriminating function of generic functions.
   (:file "list-utilities")
   ;; Define ordinary functions ENSURE-CLASS, ENSURE-BUILT-IN-CLASS,
   ;; ENSURE-GENERIC-FUNCTION, and ENSURE-METHOD to call
   ;; *ENSURE-CLASS, *ENSURE-BUILT-IN-CLASS, *ENSURE-GENERIC-FUNCTION,
   ;; and *ENSURE-METHOD of phase 2, meaning these functions create
   ;; ersatz instances by instantiating bridge classes. 
   (:file "ensure")
   ;; Define ordinary functions to be used by the expansion code for
   ;; DEFCLASS.  These functions are responsible for checking the
   ;; syntax of the DEFCLASS forms, and for canonicalizing superclass
   ;; specifications, slot specifications, and class options.
   (:file "defclass-support")
   ;; Define the macro SICL-BOOT-PHASE3:DEFCLASS.  It expands to a
   ;; call to ENSURE-CLASS.  The symbol SICL-BOOT-PHASE3:ENSURE-CLASS
   ;; names a function that calls SICL-BOOT-PHASE2:*ENSURE-CLASS,
   ;; which means that this DEFCLASS will create an ersatz class by
   ;; instantiating a bridge class. 
   (:file "defclass-defmacro")
   ;; Define the macro SICL-BOOT-PHASE3:DEFINE-BUILT-IN-CLASS.  It
   ;; expands to a call to ENSURE-BUILT-IN-CLASS.  The symbol
   ;; SICL-BOOT-PHASE3:ENSURE-BUILT-IN-CLASS names a function that
   ;; calls SICL-BOOT-PHASE2:*ENSURE-BUILT-IN-CLASS, which means that
   ;; this DEFINE-BUILT-IN-CLASS will create an built-in ersatz class
   ;; by instantiating a bridge class.
   (:file "define-built-in-class-defmacro")
   ;; Define the macro SICL-BOOT-PHASE3:DEFGENERIC.  It expands to a
   ;; call to ENSURE-GENERIC-FUNCTION.  The symbol
   ;; SICL-BOOT-PHASE3:ENSURE-GENERIC-FUNCTION names a function that
   ;; calls SICL-BOOT-PHASE3:*ENSURE-GENERIC-FUNCTION, which means
   ;; that DEFGENERIC will create an ersatz generic function by
   ;; instantiating a bridge class.
   (:file "defgeneric-defmacro")
   ;; Define MAKE-METHOD-LAMBDA as an ordinary function.
   ;; MAKE-METHOD-LAMBDA is called by the expansion code of DEFMETHOD
   ;; in order to turn the method body into a lambda expression
   ;; suitable for method invocation. 
   (:file "make-method-lambda-support")
   (:file "make-method-lambda-defuns")
   ;; Define ordinary functions to be used by the expansion code for
   ;; DEFMETHOD.  These functions are responsible for checking the
   ;; syntax of the DEFMETHOD forms, and for canonicalizing the list
   ;; of specializers.
   (:file "defmethod-support")
   ;; Define the macro SICL-BOOT-PHASE3:DEFMETHOD.  It expands to a
   ;; call to ENSURE-METHOD.  The symbol
   ;; SICL-BOOT-PHASE3:ENSURE-METHOD names a function that calls
   ;; SICL-BOOT-PHASE3:*ENSURE-METHOD, which means that DEFGMETHOD
   ;; will create an ersatz method by instantiating a bridge class.
   (:file "defmethod-defmacro")
   ;; Load the hierarchy of MOP classes, which will create ersatz
   ;; classes, ersatz generic functions, ersatz methods, and ersatz
   ;; slot definitions.  All of these are instances of bridge classes.
   (:file "mop-class-hierarchy")
   ;; Load the classes for manipulating global environments, which
   ;; will create ersatz classes, ersatz generic functions, etc., all
   ;; of which are instances of bridge classes.
   (:file "environment-classes")
   ;; Load the built-in class PACKAGE which will create an ersatz
   ;; class, ersatz generic functions, etc., all of which are
   ;; instances of bridge classes.
   (:file "package-class")
   ;; Load the built-in class SYMBOL which will create an ersatz
   ;; class, ersatz generic functions, etc., all of which are
   ;; instances of bridge classes.
   (:file "symbol-class")
   ;; Load the built-in class SEQUENCE which will create an ersatz
   ;; class which is an instance of a bridge class.
   (:file "sequence-class")
   ;; Load the ersatz built-in class ARRAY and its subclasses.
   (:file "array-classes")
   ;; Finalize all the ersatz classes we have defined so far.  FIXME:
   ;; This step may actually not be necessary.
   (:file "finalize-all-ersatz-classes")
   ;; Patch all ersatz instances created so far by calling the function 
   ;; SICL-BOOT-PHASE2:PATCH-ERSATZ-OBJECTS from phase 2.
   ;; FIXME: does that include the global environment we just created?
   (:file "patch-all-ersatz-objects")
   ;; We define the support code for SHARED-INITIALIZE again here,
   ;; because this time it needs to use bridge generic functions to
   ;; access ersatz classes in order to initialize ersatz instances.
   (:file "shared-initialize-support")
   ;; Update the variable named *MAKE-INSTANCE-DEFAULT* to contain the
   ;; function named SICL-BOOT-PHASE2:MAKE-INSTANCE-DEFAULT and the
   ;; variable named *SHARED-INITIALIZE-DEFAULT* to contain the
   ;; function name SICL-BOOT-PHASE3:SHARED-INITIALIZE-DEFAULT defined
   ;; above.  This means that MAKE-INSTANCE and SHARED-INITIALIZE will
   ;; now access ersatz classes in order to initialize ersatz
   ;; instances.
   (:file "update-functions")
   ;; Create an ersatz instance of the bridge class GLOBAL-ENVIRONMENT
   ;; and store the instance in
   ;; SICL-GLOBAL-ENVIRONMENT:*GLOBAL-ENVIRONMENT*.
   (:file "global-environment")
   ;; Here in phase 3, ENSURE-CLASS, ENSURE-BUILT-IN-CLASS,
   ;; ENSURE-GENERIC-FUNCTION, and ENSURE-METHOD are symbols that mean
   ;; that an ersatz class, an ersatz generic function, or an ersatz
   ;; method should be created from a bridge class.  In phase 4, we
   ;; need for these names to mean that an ersatz instance should be
   ;; created from an ersatz class.  However, the computations to
   ;; obtain that effect belong here in phase 3.  For that reason, we
   ;; define ordinary functions *ENSURE-CLASS, *ENSURE-BUILT-IN-CLASS,
   ;; *ENSURE-GENERIC-FUNCTION, and *ENSURE-METHOD here, and that are
   ;; called from the analogous ENSURE-... functions in phase 3.
   (:file "xensure-class")
   (:file "xensure-built-in-class")
   (:file "xensure-generic-function")
   (:file "xensure-method")
   ;; Remove the nickname SICL-CLOS from the package SICL-BOOT-PHASE3.
   (:file "rename-package-2")))
