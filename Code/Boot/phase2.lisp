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
;;;; These instances will be added to certain slots of the bridge
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

;;; Define a special version of ENSURE-METHOD for phase 2.  Three
;;; environments are involved.  ENV1 is the environment in which the
;;; function ENSURE-METHOD will be defined.  ENV2 is the environment
;;; in which the name of the function to add a method to is defined.
;;; ENV3 is the environment in which class names (specializers) are
;;; associated with class metaobjects.
;;;
;;; Notice that the name ENSURE-METHOD is a symbol in the SICL-BOOT
;;; package, rather than in the SICL-CLOS package.  It makes no
;;; difference where we put it because it is only going to be used in
;;; the expansion of DEFMETHOD which is also defined here.
(defun define-ensure-method-phase2 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'ensure-method env1)
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

;;; Define a special version of the macro DEFMETHOD for phase 2.  This
;;; special version only works for defining instances of
;;; STANDARD-METHOD on instances of STANDARD-GENERIC-FUNCTION.  Rather
;;; than using the full generality of MAKE-METHOD-LAMBDA, we just use
;;; MAKE-METHOD-LAMBDA-DEFAULT which does exactly what is required for
;;; an instance of STANDARD-METHOD on a STANDARD-GENERIC-FUNCTION.
;;;
;;; Notice that the expansion contains a call to ENSURE-METHOD, which
;;; is a symbol in the package SICL-BOOT, as opposed to the package
;;; SICL-CLOS.  Therefore this macro only works together with the
;;; version of ENSURE-METHOD defined above.
(defun define-defmethod-phase2 (env)
  (setf (sicl-genv:macro-function 'defmethod env)
	(lambda (form environment)
	  (declare (ignore environment))
	  (destructuring-bind (function-name . rest)
	      (rest form)
	    (multiple-value-bind
		  (qualifiers lambda-list specializers
		   declarations documentation forms)
		(sicl-clos::parse-defmethod rest)
	      (let ((function (sicl-clos::make-method-lambda-default
			       nil nil
			       `(lambda ,lambda-list
				  ,@declarations
				  ,@forms)
			       nil)))
		`(ensure-method
		  ',function-name
		  ',lambda-list
		  ',qualifiers
		  ,(sicl-clos::canonicalize-specializers specializers)
		  ,documentation
		  ,function)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating class accessor generic functions.

;;; We define a special version of ENSURE-GENERIC-FUNCTION in the
;;; run-time environment to be used in phase 2.  This version of
;;; ENSURE-GENERIC-FUNCTION is defined in ENV1 and operates in ENV2.
;;; It checks whether there is already a function named FUNCTION-NAME
;;; in ENV2.  If so that function is returned, and it is assumed to be
;;; a generic function.  If not, an instance of the class
;;; STANDARD-GENERIC-FUNCTION in ENV3 is created and associated with
;;; FUNCTION-NAME in ENV2.
(defun define-ensure-generic-function-phase2 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'ensure-generic-function env1)
	(lambda (function-name &rest arguments)
	  (let ((args (copy-list arguments)))
	    (loop while (remf args :environment))
	    (if (sicl-genv:fboundp function-name env2)
		(sicl-genv:fdefinition function-name env2)
		(setf (sicl-genv:fdefinition function-name env2)
		      (apply #'make-instance
			     (sicl-genv:find-class 'standard-generic-function
						   env3)
			     :method-class
			     (sicl-genv:find-class 'standard-method
						   env3)
			     :name function-name
			     args)))))))

;;; Define the macro DEFGENERIC for use in phase 2.  We define it a
;;; bit differently from its usual definition.  It is defined in the
;;; environment ENV.  The expansion defines a generic function in the
;;; environment in which the form is executed.  However, before
;;; definining it, we remove the existing generic function if it
;;; exists.  This way, we are sure to get a fresh generic function, as
;;; opposed to one that happened to have been imported from the host.
;;; We must, of course, make sure that we execute a DEFGENERIC form
;;; for a particular generic function exactly once, but we can do that
;;; because we completely master the boot process.
(defun define-defgeneric-phase2 (env)
  (setf (sicl-genv:macro-function 'defgeneric env)
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(second form)
					 (sicl-genv:global-environment))
		  (setf (sicl-genv:fdefinition ',(second form)
					       (sicl-genv:global-environment))
			(ensure-generic-function
			 ',(second form)
			 :name ',(second form)
			 :lambda-list ',(third form)))))))

(defun phase2 (boot)
  (let ((r1 (r1 boot))
	(r2 (r2 boot))
	(r3 (r3 boot)))
    (message "Start of phase 2~%")
    (define-ensure-method-phase2 r1 r1 r2)
    (define-defmethod-phase2 r1)
    (ld "../CLOS/generic-function-initialization-support.lisp" r1 r1)
    (ld "../CLOS/invalidate-discriminating-function.lisp" r1 r1)
    (ld "../CLOS/generic-function-initialization-defmethods.lisp" r1 r1)
    (define-ensure-generic-function-phase2 r3 r3 r2)
    (define-defgeneric-phase2 r3)
    (ld "../CLOS/accessor-defgenerics.lisp" r3 r3)
    ;; (create-bridge-class-accessors boot)
    ;; (ld "../CLOS/add-remove-direct-subclass-defmethods.lisp" c r)
    ;; (ld "../CLOS/add-accessor-method.lisp" c r)
    ;; (ld "../CLOS/class-initialization-support.lisp" c r)
    ;; (setf (fdefinition
    ;; 	   'sicl-clos:shared-initialize-around-real-class-default)
    ;; 	  (sicl-genv:fdefinition
    ;; 	   'sicl-clos:shared-initialize-around-real-class-default
    ;; 	   r))
    ;; (ld "../CLOS/class-initialization-defmethods.lisp" c r)
    ;; (ld "../CLOS/ensure-class-using-class-support.lisp" c (r2 boot))
    ;; (ld "ensure-class-defun-phase2.lisp" c (r2 boot))
    ;; (create-bridge-classes boot)
    (message "End of phase 2~%")))

;;  LocalWords:  accessor metaobject metaobjects canonicalized
;;  LocalWords:  accessors instantiation specializer superclass
