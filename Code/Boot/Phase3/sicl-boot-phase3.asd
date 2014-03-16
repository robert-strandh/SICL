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
   (:file "define-built-in-class-defmacro")
   (:file "defgeneric-defmacro")
   (:file "make-method-lambda-support")
   (:file "make-method-lambda-defuns")
   (:file "defmethod-support")
   (:file "defmethod-defmacro")
   (:file "mop-class-hierarchy")
   (:file "environment-classes")
   (:file "finalize-all-target-classes")
   (:file "global-environment")
   (:file "patch-all-target-objects")
   (:file "shared-initialize-support")
   (:file "update-functions")
   (:file "xensure-class")
   (:file "xensure-built-in-class")
   (:file "xensure-generic-function")
   (:file "xensure-method")
   (:file "rename-package-2")))
