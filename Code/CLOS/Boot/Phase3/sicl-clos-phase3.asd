(cl:in-package #:common-lisp-user)

;;;; When phase 3 starts, we have a bunch of bridge generic functions
;;;; in the association list *BRIDGE-GENERIC-FUNCTIONS* and a bunch of
;;;; bridge classes in the association list *BRIDGE-CLASSES*.  The
;;;; bridge generic functions are only the accessor functions for
;;;; which accessor methods were automatically created when the bridge
;;;; classes were created.  The bridge classes correspond to the class
;;;; hierarchy specified in the AMOP.
;;;;
;;;; The purpose of phase 3 is repeat the action of phase 2, except
;;;; that target generic functions and target classes will be created.
;;;; For that to work, the bridge classes, which are host instances
;;;; but not host classes, must be able to play the role of classes,
;;;; i.e., it must be possible to instantiate them by calling some
;;;; version of MAKE-INSTANCE.
;;;;
;;;; We accomplish the task in three stages.  First, we create more
;;;; bridge generic functions, namely MAKE-INSTANCE and those generic
;;;; functions that are needed by MAKE-INSTANCE such as
;;;; ALLOCATE-INSTANCE, INITIALIZE-INSTANCE, etc.  Next, we INSTALL
;;;; the bridge generic functions that are needed as host functions so
;;;; that they can be called normally.  Finally, we use what was
;;;; installed to create the standard accessor generic functions as
;;;; target generic functions and to create the standard classes as
;;;; target classes.

(asdf:defsystem :sicl-clos-phase3
  :depends-on (:sicl-clos-phase1 :sicl-clos-phase2)
  ;; We use :SERIAL T so as to reduce the clutter with respect to the
  ;; dependencies, and to make the order completely predictable.
  :serial t
  :components
  (;; When this phase starts, we have a bunch of bridge classes in
   ;; *BRIDGE-CLASSES*.  It is convenient to finalize them all so that
   ;; we are done with some of the accessor functions. 
   (:file "finalize-classes")
   ;; Define a heap instance as a STRUCT with two slots, the class and
   ;; the contents vector.  During this phase, the class slot will
   ;; contain a bridge class.
   (:file "heap-instance")
   ;; Define a version of CLASS-OF that, if given a heap instance as
   ;; defined previously, returns the contents of the class slot.  If
   ;; given something else, it returns the bridge class named T.
   (:file "class-of")
   ;; Define an ordinary function that implements the default action
   ;; for ALLOCATE-INSTANCE when given a standard class or a
   ;; funcallable standard class.  The default action is to allocate a
   ;; heap instance in the form of a STRUCT and a contents vector that
   ;; contains 2 more cells than there are slots in the class so as to
   ;; make room for the class number and the list of effective slots
   ;; of the class of the instance. 
   (:file "allocate-instance-support")
   ;; Define an ordinary function called ALLOCATE-INSTANCE that only
   ;; works correctly when given a standard class or a funcallable
   ;; standard class.
   (:file "allocate-instance-defuns")
   ;; Define ALLOCATE-BUILT-IN-INSTANCE as an ordinary function.  It
   ;; allocates a heap instance in the form of a STRUCT and a contents
   ;; vector that contains 1 more cell than there are slots in the
   ;; class so as to make room for the class number.
   (:file "allocate-built-in-instance")
   ;; Define the two ordinary functions STANDARD-INSTANCE-ACCESS and
   ;; (SETF STANDARD-INSTANCE-ACCESS) according to the specification.
   ;; They take an instance and a location in the form of an integer,
   ;; and return (or set) the contents of the cell in the contents
   ;; vector with the given location.
   (:file "standard-instance-access")
   ;; Define ordinary functions that implement default behavior of
   ;; SLOT-VALUE-USING-CLASS and others in the same family.
   (:file "slot-value-etc-support")
   ;; Define SLOT-VALUE-USING-CLASS and others in the same family as
   ;; ordinary functions.  They work if the class of the object is
   ;; either STANDARD-CLASS or FUNCALLABLE-STANDARD-CLASS.
   (:file "slot-value-etc-defuns")
   ;; During phase 2, SLOT-VALUE and (SETF SLOT-VALUE) were defined to
   ;; always call ERROR, but they were defined so as to avoid compiler
   ;; warnings when accessor methods referred to them.  Now, we
   ;; undefine them, in order to avoid a compiler warning in the next
   ;; step.
   (:file "fmakunbound-slot-value")
   ;; Define specified functions in the SLOT-VALUE family. 
   (:file "slot-value-etc-specified-defuns")
   ;; Define ordinary functions implementing standard actions for
   ;; INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and
   ;; SHARED-INITIALIZE.
   (:file "initialize-support")
   ;; Define generic functions INITIALIZE-INSTANCE,
   ;; REINITIALIZE-INSTANCE, and SHARED-INITIALIZE.  They will be
   ;; created as bridge generic functions and added to the list in
   ;; *BRIDGE-GENERIC-FUNCTIONS*, because DEFGENERIC still works that
   ;; way.
   (:file "initialize-defgenerics")
   ;; Define default primary methods on the generic functions
   ;; INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and
   ;; SHARED-INITIALIZE.  These methods are specialized to
   ;; STANDARD-OBJECT.
   (:file "initialize-defmethods")
   ;; Define :AFTER methods on INITIALIZE-INSTANCE and
   ;; REINITIALIZE-INSTANCE specialized to STANDARD-GENERIC-FUNCTION
   ;; that call the default action already defined during phase 2.
   (:file "generic-function-initialization-defmethods")
   ;; Define :AFTER methods on INITIALIZE-INSTANCE specialized to
   ;; STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS, and BUILT-IN-CLASS
   ;; that call the default action already defined during phase 2.
   (:file "class-initialization-defmethods")
   ;; Define :AFTER methods on INITIALIZE-INSTANCE specialized to
   ;; METHOD and STANDARD-ACCESSOR-METHOD that call the default action
   ;; already defined during phase 2.
   (:file "method-initialization-defmethods")
   ;; Define functions, generic functions and methods for
   ;; INITIALIZE-BUILT-IN-INSTANCE that are analogous to
   ;; INITIALIZE-INSTANCE, but they work for instances of built-in
   ;; classes.
   (:file "built-in-initialize-support")
   (:file "built-in-initialize-defgenerics")
   (:file "built-in-initialize-defmethods")
   (:file "add-remove-direct-method-defgenerics")
   (:file "add-remove-direct-method-defmethods")
   ;; Up until now, defining generic functions and methods (using
   ;; DEFGENERIC and DEFMETHOD) meant adding them to the contents of
   ;; the list *BRIDGE-GENERIC-FUNCTIONS*, but their names were either
   ;; not fbound or fbound to host generic functions.  Now is the time
   ;; to start working on the purpose of phase 3, namely to define
   ;; instances of bridge classes.  The generic functions that allow
   ;; us to do that must be fbound so that they can be called
   ;; normally.  The purpose of this step is to remove those generic
   ;; functions from *BRIDGE-GENERIC-FUNCTIONS* and make them fbound.
   ;; Not all bridge generic functions are moved; only those that are
   ;; required to make instances of bridge classes.  Accessors for the
   ;; finalized part of classes, such as CLASS-SLOTS must still take
   ;; bridge classes as arguments, so they remain defined as host
   ;; generic functions.
   ;; 
   ;; Once this step is performed, we can no longer add any bridge
   ;; generic functions by instantiating host classes, because the
   ;; accessor functions required to do that have been destroyed. 
   (:file "install-generic-functions")
   ;; Define an ordinary function implementing the default behavior of
   ;; MAKE-INSTANCE.  It works for standard classes and funcallable
   ;; standard classes.
   (:file "make-instance-support")
   ;; Up until now, MAKE-INSTANCE was an alias for the host
   ;; MAKE-INSTANCE function, because it was used to create bridge
   ;; classes, bridge generic functions, etc, which are instances of
   ;; host classes.  In preparation for redefining MAKE-INSTANCE to
   ;; make instances of bridge classes, we first make it funbound so
   ;; as to avoid a compiler warning.
   (:file "fmakunbound-make-instance")
   ;; Create MAKE-INSTANCE as an ordinary function that can accept a
   ;; symbol or a bridge class.  It calls the function
   ;; MAKE-INSTANCE-DEFAULT with the class and the remaining arguments.
   (:file "make-instance")
   ;; When we define generic functions in this phase, they are target
   ;; generic functions, so we need a new database for them.  We use
   ;; an association list which is the value of
   ;; *TARGET-GENERIC-FUNCTIONS*.
   (:file "make-built-in-instance-support")
   (:file "make-built-in-instance")
   (:file "generic-function-database")
   ;; Define a version of the DEFGENERIC macro that instantiates the
   ;; bridge class STANDARD-GENERIC-FUNCTION and adds the result to
   ;; the database of target generic functions.
   (:file "defgeneric-defmacro")
   ;; Redefine COMPILE to call the cross compiler so that the
   ;; discriminating function of newly-created target generic
   ;; functions becomes a target function.
   (:file "compile")
   ;; Define all the standard accessors as target generic
   ;; functions.
   (:file "accessor-defgenerics")
   ;; When we define classes in this phase, they are target classes,
   ;; so we need a new database for them.  We use an association list
   ;; which is the value of *TARGET-CLASSES*.
   (:file "class-database")
   ;; In preparation for executing a bunch of DEFINE-BUILT-IN-CLASS
   ;; forms for creating target classes, we must replace the version
   ;; of ENSURE-BUILT-IN-CLASS used in phase 2 (for creating bridge
   ;; classes) by a new version that creates target classes.
   (:file "ensure-built-in-class")
   ;; In preparation for executing a bunch of DEFCLASS forms for
   ;; creating target classes, we must replace the version of
   ;; ENSURE-CLASS used in phase 2 (for creating bridge classes) by a
   ;; new version that creates target classes.
   (:file "ensure-class")
   ;; In preparation for executing a bunch of DEFMETHOD forms for
   ;; creating target methods, we must replace the version of
   ;; ENSURE-METHOD used in phase 2 (for creating methods on bridge
   ;; generic functions) by a new version that creates methods on
   ;; target generic functions.
   (:file "ensure-method")
   ))
