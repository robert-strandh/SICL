(cl:in-package #:sicl-boot)

;;;; The main purpose of phase 2 is to create a mirror of the MOP
;;;; class hierarchy in run-time environment R2.  We call the classes
;;;; in this hierarchy BRIDGE CLASSES.  A bridge class is neither a
;;;; host class nor a target class.  Instead, a bridge class is an
;;;; instance of one of the host classes we created in phase 1.
;;;; However, a bridge class behaves pretty much like a target class
;;;; in that we have accessor functions that let us query all aspects
;;;; of it as if it were a target class.
;;;;
;;;; To create a bridge class, we need to instantiate a host class.
;;;; Therefore, calling MAKE-INSTANCE in the run-time environment R2
;;;; ultimately calls the host function with the same name.  However,
;;;; if MAKE-INSTANCE is invoked with the name of a class, rather than
;;;; with a class metaobject, then we need for that name to mean the
;;;; name it was given during phase 1.  Therefore, in the run-time
;;;; environment, MAKE-INSTANCE is associated with a temporary
;;;; function that looks up the name in the run-time environment R1
;;;; before calling the host function named make-instance.
;;;;
;;;; The main difficulty with phase 2 is that we need to implement the
;;;; class initialization protocol.  This protocol mainly works on the
;;;; classes metaobjects themselves.  So for instance, it is the
;;;; responsibility of the class initialization protocol to convert
;;;; canonicalized slot descriptors to direct-slot-definition
;;;; metaobjects and associate them with the class metaobjects.
;;;;
;;;; However, the class initialization protocol is also in charge of
;;;; adding accessor methods to generic functions that are mentioned
;;;; in the slot descriptors of the DEFCLASS forms.  These generic
;;;; functions and methods do not take bridge classes as arguments,
;;;; but INSTANCES of bridge classes that will be created in a later
;;;; phase.  We are thus dealing with two different families of
;;;; accessor generic functions with the same names during phase 2.
;;;; The first family consists of HOST GENERIC FUNCTIONS that were
;;;; created before bootstrapping commenced, and that had methods
;;;; added to them during phase 1.  Arguments to these generic
;;;; functions are bridge classes.  The second family is called BRIDGE
;;;; GENERIC FUNCTIONS and they are created as a result of bridge
;;;; classes being created through the instantiation of host classes
;;;; created in phase 1.  To avoid clashes between these two families
;;;; of generic functions, we make the names map to host generic
;;;; functions in run-time-environment R2, and we make the names map
;;;; to bridge generic functions in the run-time environment R3.
;;;;
;;;; The class initialization protocol is triggered by auxiliary
;;;; methods on the functions INITIALIZE-INSTANCE,
;;;; REINITIALIZE-INSTANCE, and SHARED-INITIALIZE.  Since we are
;;;; initializing ordinary host classes, these functions are those of
;;;; the host.  Therefore, in phase 2, DEFMETHOD translates to a host
;;;; DEFMETHOD after the names of the specializer classes have been
;;;; translated to the names that the host knows them by.

(defun phase2 (boot)
  (let ((c (c1 boot))
	(r (r2 boot)))
    (create-bridge-class-accessors boot)
    (ld "../CLOS/add-remove-direct-subclass-defmethods.lisp" c r)
    (ld "../CLOS/add-accessor-method.lisp" c r)
    (ld "../CLOS/class-initialization-support.lisp" c r)
    (setf (fdefinition
	   'sicl-clos:shared-initialize-around-real-class-default)
	  (sicl-genv:fdefinition
	   'sicl-clos:shared-initialize-around-real-class-default
	   (r2 boot)))
    (setf (fdefinition
	   'sicl-clos:initialize-instance-after-built-in-class-default)
	  (sicl-genv:fdefinition
	   'sicl-clos:initialize-instance-after-built-in-class-default
	   (r2 boot)))
    (setf (fdefinition
	   'sicl-clos:initialize-instance-after-regular-class-default)
	  (sicl-genv:fdefinition
	   'sicl-clos:initialize-instance-after-regular-class-default
	   (r2 boot)))
    (ld "../CLOS/class-initialization-defmethods.lisp" c r)))

;;  LocalWords:  accessor metaobject metaobjects canonicalized
;;  LocalWords:  accessors instantiation specializer
