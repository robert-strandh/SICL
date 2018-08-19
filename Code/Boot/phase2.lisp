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
;;; than using the full generality of MAKE-METHOD-LAMBDA, we just do
;;; exactly what is required for an instance of STANDARD-METHOD on a
;;; STANDARD-GENERIC-FUNCTION.
;;;
;;; Notice that the expansion contains a call to ENSURE-METHOD, which
;;; is a symbol in the package SICL-BOOT, as opposed to the package
;;; SICL-CLOS.  Therefore this macro only works together with the
;;; version of ENSURE-METHOD defined above.
;;;
;;; ENV1 is the environment in which the macro is defined.  ENV2 is
;;; the environment in which we look up the function named
;;; SICL-CLOS:METHOD-FUNCTION.
(defun define-defmethod-phase2 (env1 env2)
  (setf (sicl-genv:macro-function 'defmethod env1)
	(lambda (form environment)
	  (declare (ignore environment))
	  (destructuring-bind (function-name . rest)
	      (rest form)
	    (multiple-value-bind
		  (qualifiers required remaining specializers
		   declarations documentation forms)
		(sicl-clos::parse-defmethod rest)
	      (let ((lambda-list (append required remaining))
                    (args (gensym))
		    (next-methods (gensym)))
		`(ensure-method
		  ',function-name
		  ',lambda-list
		  ',qualifiers
		  ,(sicl-clos::canonicalize-specializers specializers)
		  ,documentation
		  (lambda (,args ,next-methods)
		    (flet ((next-method-p ()
			     (not (null ,next-methods)))
			   (call-next-method (&rest args)
			     (when (null ,next-methods)
			       (error "no next method"))
			     (funcall (funcall (sicl-genv:fdefinition
						'sicl-clos:method-function
						,env2)
					       (car ,next-methods))
				      (or args ,args)
				      (cdr ,next-methods))))
		      (apply (lambda ,lambda-list
			       ,@declarations
			       ,@forms)
			     ,args))))))))))

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
;;; FUNCTION-NAME in ENV2.  We must pass an explicit value of
;;; :METHOD-CLASS to the host function MAKE-INSTANCE, and we look up
;;; that value as the class named STANDARD-METHOD in ENV3.  Failing to
;;; do so would make the generic-function initialization protocol
;;; provide a default value, and it would be the wrong one.
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
(defun define-defgeneric-phase2 (env1 env2)
  (setf (sicl-genv:macro-function 'defgeneric env1)
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(second form) ,env2)
		  (setf (sicl-genv:fdefinition ',(second form) ,env2)
			(ensure-generic-function
			 ',(second form)
			 :name ',(second form)
			 :lambda-list ',(third form)))))))

;;; When the class-initialization protocol turns a canonicalized slot
;;; specification into a DIRECT-SLOT-DEFINITION metaobject, it calls
;;; the function DIRECT-SLOT-DEFINITION-CLASS, passing it the class
;;; metaobject and some other arguments.  For simplicity, we define a
;;; special version of that function here.  It always returns the
;;; class named STANDARD-DIRECT-SLOT-DEFINITION.
(defun define-direct-slot-definition-class-phase2 (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class env1)
	(lambda (&rest arguments)
	  (declare (ignore arguments))
	  (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition
				env2))))

;;; Recall that the functions READER-METHOD-CLASS and
;;; WRITER-METHOD-CLASS are called as part of the class-initialization
;;; protocol as described in this section
;;; http://metamodular.com/CLOS-MOP/initialization-of-class-metaobjects2.html
;;; of the AMOP.
(defun define-reader-method-class-phase2 (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class env1)
	(lambda (&rest arguments)
	  (declare (ignore arguments))
	  (sicl-genv:find-class 'sicl-clos:standard-reader-method env2))))

(defun define-writer-method-class-phase2 (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class env1)
	(lambda (&rest arguments)
	  (declare (ignore arguments))
	  (sicl-genv:find-class 'sicl-clos:standard-writer-method env2))))

;;; This function is in charge of making sure that all the MOP
;;; accessor generic functions are properly defined as bridge generic
;;; functions in phase 2.
(defun define-accessor-generic-functions-phase2 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'sicl-clos::compute-discriminating-function env2)
        (lambda (generic-function) (declare (ignore generic-function)) nil))
  (ld "CLOS/invalidate-discriminating-function.lisp" env2 env2)
  ;; Before we can start creating generic functions, we must make
  ;; sure that the generic-function initialization protocol is
  ;; enabled.
  (ld "CLOS/generic-function-initialization-support.lisp" env2 env2)
  (sicl-extrinsic-environment:import-function-from-host 'shared-initialize env2)
  (ld "CLOS/generic-function-initialization-defmethods.lisp" env2 env2)
  ;; We must also make sure that DEFGENERIC is handled properly for
  ;; phase 2.
  (define-ensure-generic-function-phase2 env2 env3 env1)
  (define-defgeneric-phase2 env2 env3)
  ;; Define the accessor generic functions.
  (ld "CLOS/accessor-defgenerics.lisp" env2 env2))

;;; Define ENSURE-CLASS to be an alias for the default function
;;; ENSURE-CLASS-USING-CLASS-NULL.
(defun define-ensure-class-phase2 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class env1)
	(lambda (name
		 &rest keys
		 &key
		   direct-default-initargs
		   direct-slots
		   ((:direct-superclasses direct-superclass-names))
		   ((:metaclass metaclass-name) nil metaclass-p)
		 &allow-other-keys)
	  (unless metaclass-p
	    (setf metaclass-name 'standard-class))
	  (let ((remaining-keys (copy-list keys))
		(metaclass (sicl-genv:find-class metaclass-name env2))
		(direct-superclasses
		  (loop for name in direct-superclass-names
			collect (sicl-genv:find-class name env3))))
	    (loop while (remf remaining-keys :metaclass))
	    (loop while (remf remaining-keys :direct-superclasses))
	    (setf (sicl-genv:find-class name env3)
		  (apply #'make-instance metaclass
			 :direct-default-initargs direct-default-initargs
			 :direct-slots direct-slots
			 :direct-superclasses direct-superclasses
			 :name name
			 remaining-keys))))))

;;; Recall that the function DEFAULT-SUPERCLASSES is a SICL-SPECIFIC
;;; function that is called by the class-initialization protocol to
;;; determine a list of default superclasses when no superclasses are
;;; given for the creation of a class.  This AMOP section:
;;; http://metamodular.com/CLOS-MOP/initialization-of-class-metaobjects2.html
;;; describes that when the class that is being instantiated is
;;; STANDARD-CLASS, then the default superclasses is a list of a
;;; single class, namely the class named STANDARD-OBJECT, and that
;;; when the class that is being instantiated is
;;; FUNCALLABLE-STANDARD-CLASS, then the default superclasses is a
;;; list of a single class, namely the class named
;;; FUNCALLABLE-STANDARD-OBJECT.  However, in SICL, we turned that
;;; rule into a generic function called DEFAULT-SUPERCLASSES that have
;;; methods specialized to STANDARD-CLASS, and
;;; FUNCALLABLE-STANDARD-CLASS, but other methods can be added as
;;; well.
(defun define-default-superclasses-phase2 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses env1)
	(lambda (class)
	  (cond ((eq (class-of class)
		     (sicl-genv:find-class 'standard-class env2))
		 (sicl-genv:find-class 'standard-object env3))
		((eq (class-of class)
		     (sicl-genv:find-class 'sicl-clos:funcallable-standard-class
					   env2))
		 (sicl-genv:find-class 'sicl-clos:funcallable-standard-object
				       env3))
		(t
		 '())))))

(defun define-validate-superclass-phase2 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass env)
	(constantly t)))

(defun define-check-direct-superclasses-phase2 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos::check-direct-superclasses env)
	(constantly t)))

(defun define-heap-instance-p (env)
  (setf (sicl-genv:fdefinition 'sicl-clos::heap-instance-p env)
	(constantly t)))

(defun define-all-descendants-phase2 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos::all-descendants env)
	(constantly t)))

(defun define-add-method-phase2 (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:add-method env1)
	(lambda (generic-function method)
	  (funcall (sicl-genv:fdefinition
		    '(setf sicl-clos:generic-function-methods)
		    env2)
		   (cons method
			 (funcall (sicl-genv:fdefinition
				   'sicl-clos:generic-function-methods
				   env2)
				  generic-function))
		   generic-function))))

(defun phase2 ()
  (let ((r1 *phase1-mop-class-env*)
	(r2 *phase2-mop-class-env*)
	(r3 *phase2-mop-accessor-env*))
    (message "Start of phase 2~%")
    ;; In order to make it possible to execute the initialization
    ;; protocols for generic functions, methods, classes, and slot
    ;; definitions, we must be able to define host methods on host
    ;; generic functions such as INITIALIZE-INSTANCE and
    ;; SHARED-INITIALIZE.  For that purpose, we define appropriate
    ;; versions of ENSURE-METHOD and DEFMETHOD.
    (define-ensure-method-phase2 r2 r2 r1)
    (setf (sicl-genv:fdefinition 'sicl-clos:method-function
                                 r1)
          (fdefinition 'closer-mop:method-function))
    (define-defmethod-phase2 r2 r1)
    (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function
                                 r2)
          (fdefinition 'closer-mop:set-funcallable-instance-function))
    ;; Do everything necessary to define all the MOP accessor generic
    ;; functions.
    (define-accessor-generic-functions-phase2 r1 r2 r3)
    (define-direct-slot-definition-class-phase2 r2 r1)
    (sicl-extrinsic-environment:import-function-from-host 'initialize-instance r2)
    (ld "CLOS/slot-definition-initialization-defmethods.lisp" r2 r2)
    (ld "CLOS/add-remove-direct-subclass-support.lisp" r2 r2)
    (ld "CLOS/add-remove-direct-subclass-defuns.lisp" r2 r2)
    (sicl-extrinsic-environment:import-function-from-host 'add-method r2)
    (sicl-extrinsic-environment:import-function-from-host 'make-instance r2)
    (ld "CLOS/add-accessor-method.lisp" r2 r2)
    (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass
                                 r2)
          (lambda (class1 class2) (declare (ignore class1 class2)) t))
    (ld "CLOS/class-initialization-support.lisp" r2 r2)
    (ld "CLOS/class-initialization-defmethods.lisp" r2 r2)
    (sicl-extrinsic-environment:import-function-from-host 'reinitialize-instance r2)
    (sicl-extrinsic-environment:import-function-from-host 'change-class r2)
    (ld "CLOS/ensure-class-using-class-support.lisp" r2 r2)
    (define-ensure-class-phase2 r2 r1 r2)
    (define-default-superclasses-phase2 r2 r2 r2)
    (define-validate-superclass-phase2 r2)
    (define-check-direct-superclasses-phase2 r2)
    (define-heap-instance-p r2)
    (define-all-descendants-phase2 r2)
    (define-reader-method-class-phase2 r2 r1)
    (define-writer-method-class-phase2 r2 r1)
    (define-add-method-phase2 r2 r2)
    (ld "CLOS/class-unique-number-defparameter.lisp" r2 r2)
    (ld "CLOS/defclass-support.lisp" r2 r2)
    (ld "CLOS/defclass-defmacro.lisp" r2 r2)
    (create-mop-classes r2 r2)
    (sicl-extrinsic-environment:import-function-from-host 'print-object r2)
    (ld "Boot/print-object-defmethods-phase2.lisp" r2 r2)
    (message "End of phase 2~%")))

;;  LocalWords:  accessor metaobject metaobjects canonicalized
;;  LocalWords:  accessors instantiation specializer superclass
