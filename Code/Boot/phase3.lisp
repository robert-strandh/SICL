(cl:in-package #:sicl-boot)

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
  (let ((r2 *phase2-mop-class-env*)
	(r3 *phase2-mop-accessor-env*))
    (ld "../CLOS/class-finalization-support.lisp" r2 r2)
    (define-allocate-general-instance r3)))
