(cl:in-package #:sicl-boot)

(defun define-effective-slot-definition-class-phase3 (env1 env2)
  (setf (sicl-genv:fdefinition 'sicl-clos:effective-slot-definition-class
			       env1)
	(lambda (&rest args)
	  (declare (ignore args))
	  (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition
				env2))))

(defclass header ()
  ((%class :initarg :class :accessor class)
   (%rack :initarg :rack :reader rack)))

(defun define-allocate-general-instance-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:allocate-general-instance env)
	(lambda (class size)
	  (make-instance 'header
	    :class class
	    :rack (make-array size)))))

(defun define-general-instance-access-phase3 (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:general-instance-access env)
	(lambda (instance offset)
	  (aref (rack instance) offset))))

(defun define-setf-general-instance-access-phase3 (env)
  (setf (sicl-genv:fdefinition '(setf sicl-clos:general-instance-access) env)
	(lambda (value instance offset)
	  (setf (aref (rack instance) offset) value))))

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
    (message "End of phase 3~%")))
