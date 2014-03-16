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
   (:file "finalize-all-ersatz-classes")
   (:file "global-environment")
   (:file "patch-all-ersatz-objects")
   (:file "shared-initialize-support")
   (:file "update-functions")
   (:file "xensure-class")
   (:file "xensure-built-in-class")
   (:file "xensure-generic-function")
   (:file "xensure-method")
   (:file "rename-package-2")))
