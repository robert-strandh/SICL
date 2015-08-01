(cl:in-package #:sicl-boot)

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
	    :rack (make-array size)))))

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
		(finalize-inheritance
		  (sicl-genv:fdefinition
		   'sicl-clos:finalize-inheritance env3))
		(allocate-instance
		  (sicl-genv:fdefinition
		   'allocate-instance env3)))
	    (unless (funcall finalized-p class)
	      (funcall finalize-inheritance class))
	    (let ((result (apply allocate-instance class args)))
	      result)))))

(defun define-general-instance-p-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:general-instance-p env)
	(lambda (obj)
	  (typep obj 'header))))

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
    (ld "../CLOS/discriminating-automaton.lisp" r3 r3)
    (handler-bind
	((cleavir-env:no-function-info
	   (lambda (condition)
	     (declare (ignore condition))
	   (invoke-restart 'cleavir-generate-ast:consider-global))))
      (ld "../CLOS/discriminating-tagbody.lisp" r3 r3))
    (message "End of phase 3~%")))

;;  LocalWords:  metaobject
