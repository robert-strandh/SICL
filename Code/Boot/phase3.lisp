(cl:in-package #:sicl-boot)

;;; During bootstrapping, we need to reserve a valid host object to
;;; mean the unbound slot value.  We choose an integer so that it is
;;; easy to recognize in the inspector and the debugger.
(defparameter *unbound-value* 123123123)

(defun define-unbound-value-p-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos::unbound-value-p env)
	(lambda (x) (eql x *unbound-value*))))

(defun define-unbound-value-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos::unbound-value env)
	(lambda () *unbound-value*)))

(defun define-effective-slot-definition-class-phase3 (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:effective-slot-definition-class
			       env1)
	(lambda (&rest args)
	  (declare (ignore args))
	  (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition
				env2))))

;;; During bootstrapping, we need a way to represent instances with a
;;; structure similar to that used in the final target.  We call such
;;; instances ERSATZ INSTANCES.  Here, we represent an ersatz instance
;;; as an instance of the host class HEADER containing the class of
;;; the instance and a RACK in the form of a host simple vector.
(defclass header ()
  ((%class :initarg :class :accessor class)
   (%rack :initarg :rack :reader rack)))

(defun define-class-of-phase3 (env)
  (setf (sicl-genv:fdefinition 'class-of env) #'class))

;;; Define a special version of ALLOCATE-GENERAL-INSTANCE.  Recall
;;; that ALLOCATE-GENERAL-INSTANCE takes a class metaobject and a
;;; non-negative integer representing the number of words to be
;;; allocated for the rack.  This special version allocates an ersatz
;;; instance in the form of an instance of the host class HEADER
;;; defined above, and a host simple vector for the rack.
(defun define-allocate-general-instance-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:allocate-general-instance env)
	(lambda (class size)
	  (make-instance 'header
	    :class class
	    :rack (make-array size :initial-element *unbound-value*)))))

;;; We need special versions of the functions GENERAL-INSTANCE-ACCESS
;;; and (SETF GENERAL-INSTANCE-ACCESS), because they depend on the way
;;; general instances are represented.  During bootstrapping, a
;;; general instance is represented as an instance of the host class
;;; HEADER defined above and a rack in the form of a host simple
;;; vector.

(defun define-general-instance-access-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:general-instance-access env)
	(lambda (instance offset)
	  (aref (rack instance) offset))))

(defun define-setf-general-instance-access-phase3 (env)
  (setf (sicl-genv:fdefinition '(setf sicl-clos:general-instance-access) env)
	(lambda (value instance offset)
	  (setf (aref (rack instance) offset) value))))

(defun finalize-inheritance-phase3 (class env)
  (let ((finalize-inheritance
	  (sicl-genv:fdefinition
	   'sicl-clos:finalize-inheritance env)))
    (funcall finalize-inheritance class)))

(defun finalize-all-classes-phase3 (env1 env2 env3)
  (do-all-symbols (symbol)
    (let ((class (sicl-genv:find-class symbol env1)))
      (when (and (not (null class))
		 (or (eq (class-of class)
			 (sicl-genv:find-class
			  'standard-class env2))
		     (eq (class-of class)
			 (sicl-genv:find-class
			  'sicl-clos:funcallable-standard-class env2))))
	(finalize-inheritance-phase3 class env3)))))

;;; Define a special version of MAKE-INSTANCE.  MAKE-INSTANCE is
;;; peculiar because it calls ALLOCATE-INSTANCE which takes a class
;;; metaobject to be instantiated, and then it calls
;;; INITIALIZE-INSTANCE with the resulting instance.  During
;;; bootstrapping, a class metaobject and the instances of that class
;;; metaobject are stored in two different environments, which is why
;;; we need a special version of MAKE-INSTANCE during bootstrapping.
;;;
;;; ENV1 is the environment in which MAKE-INSTANCE will be defined.
;;; ENV2 is the environment in which class the metaobject is looked
;;; up, given the name of the class.  ENV3 is the environment to use
;;; to look up the definitions of CLASS-FINALIZED-P,
;;; FINALIZE-INHERITANCE, and ALLOCATE-INSTANCE.
;;;
;;; Since we know that during bootstrapping, MAKE-INSTANCE is always
;;; called with the NAME of a class, as opposed to with a class
;;; metaobject, we do not have to handle both those cases.
(defun define-make-instance-phase3 (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'make-instance env1)
	(lambda (class-name &rest args)
	  (let ((class (sicl-genv:find-class class-name env2))
		(finalized-p
		  (sicl-genv:fdefinition
		   'sicl-clos:class-finalized-p env3))
		(allocate-instance
		  (sicl-genv:fdefinition
		   'allocate-instance env3)))
	    (unless (funcall finalized-p class)
	      (finalize-inheritance-phase3 class env3))
	    (let ((result (apply allocate-instance class args)))
	      result)))))

(defun define-general-instance-p-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:general-instance-p env)
	(lambda (obj)
	  (typep obj 'header))))

;;; Define a special version of ENSURE-METHOD for phase 3.  Four
;;; environments are involved.  ENV1 is the environment in which the
;;; function ENSURE-METHOD will be defined.  ENV2 is the environment
;;; in which the name of the function to add a method to is defined.
;;; ENV3 is the environment in which class names (specializers) are
;;; associated with class metaobjects.  ENV4 is the environment in
;;; which the name STANDARD-METHOD is associated with the method class
;;; to be instantiated.  ENV5 is the environment in which ADD-METHOD
;;; is defined.
;;;
;;; Notice that the name ENSURE-METHOD is a symbol in the SICL-BOOT
;;; package, rather than in the SICL-CLOS package.  It makes no
;;; difference where we put it because it is only going to be used in
;;; the expansion of DEFMETHOD which is also defined here.
(defun define-ensure-method-phase3 (env1 env2 env3 env4 env5)
  (setf (sicl-genv:fdefinition 'ensure-method env1)
	(lambda (function-name
		 lambda-list
		 qualifiers
		 specializers
		 documentation
		 function)
	  (let* ((fun (sicl-genv:fdefinition function-name env2))
		 (specs (loop for specializer in specializers
			      collect (sicl-genv:find-class specializer env3)))
		 (method-class (sicl-genv:find-class 'standard-method env4))
		 (add-method (sicl-genv:fdefinition 'add-method env5))
		 (method (make-instance method-class
			  :lambda-list lambda-list
			  :qualifiers qualifiers
			  :specializers specs
			  :documentation documentation
			  :function function)))
	    (funcall add-method fun method)))))

;;; Function LDP (protected loading).  It wraps the loading of a file
;;; in a handler that invokes a restart that tells the compiler to
;;; treat all undefined functions as if they were global.  This
;;; function should only be used in exceptional cases because it is
;;; better to find undefined functions at compile time than at run
;;; time.  However, sometimes we need it.  In particular, since
;;; Cleavir is currently not smart enough to recognize self-recursive
;;; functions, we need this function in those situations.
(defun ldp (file env1 env2)
  (handler-bind
      ((cleavir-env:no-function-info
	 (lambda (condition)
	   (declare (ignore condition))
	   (invoke-restart 'cleavir-generate-ast:consider-global))))
    (ld file env1 env2)))

(defun set-specializer-profiles-phase3 (env1 env2 env3)
  (do-all-symbols (symbol)
    (when (and (sicl-genv:fboundp symbol env1)
	       (eq (class-of (sicl-genv:fdefinition symbol env1))
		   (sicl-genv:find-class 'standard-generic-function env2)))
      (funcall (sicl-genv:fdefinition
		'sicl-clos::compute-and-set-specializer-profile
		env3)
	       (sicl-genv:fdefinition symbol env1)))
    (when (and (sicl-genv:fboundp `(setf ,symbol) env1)
	       (eq (class-of (sicl-genv:fdefinition `(setf ,symbol) env1))
		   (sicl-genv:find-class 'standard-generic-function env2)))
      (funcall (sicl-genv:fdefinition
		'sicl-clos::compute-and-set-specializer-profile
		env3)
	       (sicl-genv:fdefinition `(setf ,symbol) env1)))))

(defun define-compile-phase3 (env)
  (setf (sicl-genv:fdefinition 'compile env)
	(lambda (ignore lambda-expression)
	  (declare (ignore ignore))
	  (cleavir-env:eval lambda-expression env env))))

(defun satiate-phase3 (env1 env2 env3)
  (do-all-symbols (symbol)
    (when (and (sicl-genv:fboundp symbol env1)
	       (eq (class-of (sicl-genv:fdefinition symbol env1))
		   (sicl-genv:find-class 'standard-generic-function env2)))
      (funcall (sicl-genv:fdefinition
		'sicl-clos::satiate-generic-function
		env3)
	       (sicl-genv:fdefinition symbol env1)))
    (when (and (sicl-genv:fboundp `(setf ,symbol) env1)
	       (eq (class-of (sicl-genv:fdefinition `(setf ,symbol) env1))
		   (sicl-genv:find-class 'standard-generic-function env2)))
      (funcall (sicl-genv:fdefinition
		'sicl-clos::satiate-generic-function
		env3)
	       (sicl-genv:fdefinition `(setf ,symbol) env1)))))

(defun phase3 ()
  (let ((r1 *phase1-mop-class-env*)
	(r2 *phase2-mop-class-env*)
	(r3 *phase2-mop-accessor-env*))
    (message "Start of phase 3~%")
    (ld "../CLOS/class-finalization-support.lisp" r2 r2)
    (ld "../CLOS/class-finalization-defuns.lisp" r2 r2)
    (define-effective-slot-definition-class-phase3 r2 r1)
    (define-allocate-general-instance-phase3 r2)
    (define-general-instance-access-phase3 r2)
    (define-setf-general-instance-access-phase3 r2)
    (ld "../CLOS/class-unique-number-offset-defconstant.lisp" r2 r2)
    (ld "../CLOS/allocate-instance-support.lisp" r2 r2)
    (ld "../CLOS/allocate-instance-defgenerics.lisp" r1 r1)
    (ld "../CLOS/allocate-instance-defmethods.lisp" r2 r2)
    (define-make-instance-phase3 r3 r2 r2)
    (define-compile-phase3 r2)
    (ld "../CLOS/discriminating-automaton.lisp" r3 r3)
    (define-general-instance-p-phase3 r2)
    (ldp "../CLOS/list-utilities.lisp" r2 r2)
    (ldp "../CLOS/discriminating-tagbody.lisp" r2 r2)
    (ld "../CLOS/compute-discriminating-function-support.lisp" r2 r2)
    (ld "../CLOS/classp-defgeneric.lisp" r1 r1)
    (ld "../CLOS/classp-defmethods.lisp" r2 r2)
    (ld "../CLOS/compute-applicable-methods-support.lisp" r2 r2)
    (ld "../CLOS/compute-applicable-methods-defgenerics.lisp" r1 r1)
    (ld "../CLOS/compute-applicable-methods-defmethods.lisp" r2 r2)
    (ldp "../CLOS/satiation.lisp" r2 r2)
    (ld "../CLOS/compute-discriminating-function-support-a.lisp" r2 r2)
    (ld "../CLOS/instance-slots-offset-defconstant.lisp" r3 r3)
    (define-ensure-method-phase3 r3 r3 r2 r1 r2)
    (define-defmethod-phase2 r3 r2)
    (ld "../CLOS/shared-initialize-support.lisp" r3 r3)
    (ld "../CLOS/shared-initialize-defgenerics.lisp" r2 r2)
    (ld "../CLOS/shared-initialize-defmethods.lisp" r3 r3)
    (ld "../CLOS/initialize-instance-support.lisp" r3 r3)
    (ld "../CLOS/initialize-instance-defgenerics.lisp" r2 r2)
    (ld "../CLOS/initialize-instance-defmethods.lisp" r3 r3)
    (finalize-all-classes-phase3 r2 r1 r2)
    (ld "../CLOS/invalidate-discriminating-function.lisp" r2 r2)
    (ld "../CLOS/add-remove-method-support.lisp" r2 r2)
    (set-specializer-profiles-phase3 r3 r1 r2)
    (ld "../CLOS/compute-effective-method-support.lisp" r2 r2)
    (ld "../CLOS/compute-effective-method-support-a.lisp" r2 r2)
    (ld "../CLOS/method-combination-compute-effective-method-support.lisp" r2 r2)
    (ld "../CLOS/method-combination-compute-effective-method-defuns.lisp" r2 r2)
    (ld "../CLOS/compute-effective-method-defgenerics.lisp" r1 r1)
    (ld "../CLOS/compute-effective-method-defmethods.lisp" r2 r2)
    (ld "../CLOS/compute-discriminating-function-defgenerics.lisp" r1 r1)
    (ld "../CLOS/compute-discriminating-function-defmethods.lisp" r2 r2)
    (satiate-phase3 r3 r1 r2)
    (define-class-of-phase3 r3)
    (define-unbound-value-p-phase3 r3)
    (message "End of phase 3~%")))

;;  LocalWords:  metaobject
