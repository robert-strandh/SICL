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

(defun define-allocate-general-instance (env)
  (setf (sicl-genv:fdefinition 'sicl-clos:allocate-general-instance env)
	(lambda (class size)
	  (make-instance 'header
	    :class class
	    :rack (make-array size)))))

(defun phase3 ()
  (let ((r1 *phase1-mop-class-env*)
	(r2 *phase2-mop-class-env*)
	(r3 *phase2-mop-accessor-env*))
    (message "Start of phase 3~%")
    (ld "../CLOS/class-finalization-support.lisp" r2 r2)
    (ld "../CLOS/class-finalization-defuns.lisp" r2 r2)
    (define-effective-slot-definition-class-phase3 r2 r1)
    (define-allocate-general-instance r3)
    (ld "../CLOS/allocate-instance-defgenerics.lisp" r1 r1)
    (message "End of phase 3~%")))
