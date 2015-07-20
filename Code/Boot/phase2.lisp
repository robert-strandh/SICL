(cl:in-package #:sicl-boot)

;;;; In phase 2, we create instances of the host classes that we
;;;; created in phase 1, and we use the host generic functions that we
;;;; created in phase 1 to access slots of those instances.
;;;;
;;;; We start phase 2 by creating BRIDGE GENERIC FUNCTIONS
;;;; corresponding to the MOP class accessors.  A bridge generic
;;;; function is an instance of the host class that we associated with
;;;; the name STANDARD-GENERIC-FUNCTION when we created it in phase 1.
;;;; A bridge generic function is executable as a host function due to
;;;; the fact that the host class named FUNCALLABLE-STANDARD-OBJECT is
;;;; a superclass of the host class that we associated with the name
;;;; STANDARD-GENERIC-FUNCTION when we created it in phase 1.
;;;; However, a bridge generic function is not a host generic
;;;; function.
;;;;
;;;; Once the bridge generic functions are created, we create a
;;;; hierarchy of BRIDGE CLASSES corresponding to the MOP class
;;;; hierarchy.  Notice, though, that a bridge class is not a host
;;;; class at all.  As a side effect of creating a bridge class, we
;;;; will add instances of the host class that we associated with the
;;;; name STANDARD-METHOD when we created it in phase 1, and we will
;;;; add those instances to some bridge generic function.  Also as a
;;;; side effect of creating a bridge class, we will create instances
;;;; of the host class that we associated with the name
;;;; STANDARD-DIRECT-SLOT-DEFINITION when we created it in phase 1.
;;;; These instances will be added to some certain slots of the bridge
;;;; classes.
;;;;
;;;; Since all these instances are only host STANDARD-OBJECTs with no
;;;; special status, any initialization beyond what the host is doing
;;;; by default, we must take care of ourselves.  Thus, for bridge
;;;; generic functions, we must make sure that we execute the
;;;; generic-function initialization protocol, for bridge classes, we
;;;; must make sure that we execute the class initialization protocol,
;;;; and for bridge slot definitions, we must execute the
;;;; slot-definition initialization protocol.
;;;;
;;;; These initialization protocols take the form of auxiliary methods
;;;; on the generic functions INITIALIZE-INSTANCE,
;;;; REINITIALIZE-INSTANCE, and SHARED-INITIALIZE.  Since we are
;;;; instantiating host classes using the host function MAKE-INSTANCE,
;;;; these functions are the ordinary host functions with those names.
;;;; In preparation for phase 2, we must therefore make sure that we
;;;; can create methods on these generic functions, and we must make
;;;; sure that these methods can be specialized to the classes that we
;;;; created in phase 1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make it possible to define methods specialized to the classes that
;;; were created in phase 1.
;;;
;;; We use the SICL default version of the DEFMETHOD macro.  This
;;; macro expands to a call to the SICL-specific function ENSURE-METHOD.

;;; This function is similar to FIND-CLASS.  NAME is the name of a
;;; class.  ENV is the environment to use for finding the
;;; corresponding class.  The purpose of this function is to translate
;;; class names in a DEFMETHOD form to class metaobjects.  For the
;;; most part, these class names are names that we associated with
;;; classes in phase 1.  For that reason, ENV must be the environment
;;; that was used in phase 1 for that purpose.  However, when some
;;; method parameter is not specialized, we do not want to use the
;;; class that we associated with the name T in phase 1; we want to
;;; use the host class named T so that the corresponding parameter
;;; remains "unspecialized".
(defun class-from-name (name env)
  (assert (symbolp name))
  (if (eq name 't)
      (find-class t)
      (sicl-genv:find-class name env)))

(defun define-ensure-method-phase2 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'sicl-clos::temporary-ensure-method env1)
	(lambda (function-name
		 lambda-list
		 qualifiers
		 specializers
		 documentation
		 function)
	  (let* ((fun (sicl-genv:fdefinition function-name env2))
		 (specs (loop for specializer in specializers
			      collect (class-from-name specializer env3)))
		 (method (make-instance 'standard-method
			  :lambda-list lambda-list
			  :qualifiers qualifiers
			  :specializers specs
			  :documentation documentation
			  :function function)))
	    (add-method fun method)))))

(defun phase2 (boot)
  (let ((c (c1 boot))
	(r (r1 boot)))
    (create-bridge-class-accessors boot)
    (ld "../CLOS/add-remove-direct-subclass-defmethods.lisp" c r)
    (ld "../CLOS/add-accessor-method.lisp" c r)
    (ld "../CLOS/class-initialization-support.lisp" c r)
    (setf (fdefinition
	   'sicl-clos:shared-initialize-around-real-class-default)
	  (sicl-genv:fdefinition
	   'sicl-clos:shared-initialize-around-real-class-default
	   r))
    (ld "../CLOS/class-initialization-defmethods.lisp" c r)
    (ld "../CLOS/ensure-class-using-class-support.lisp" c (r2 boot))
    (ld "ensure-class-defun-phase2.lisp" c (r2 boot))
    (create-bridge-classes boot)))

;;  LocalWords:  accessor metaobject metaobjects canonicalized
;;  LocalWords:  accessors instantiation specializer superclass
