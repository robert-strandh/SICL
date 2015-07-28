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

(defun phase3 (boot)
  (let ((c (c1 boot))
	(r (r2 boot)))
    (define-effective-slot-definition-class boot)
    (ld "../CLOS/class-finalization-support.lisp" c r)
    (export-to-host 'sicl-clos::compute-default-initargs-default r)
    (export-to-host 'sicl-clos::compute-slots-default r)
    (export-to-host 'sicl-clos::compute-slots-around-default r)
    (export-to-host 'sicl-clos::compute-effective-slot-definition-default r)
    (export-to-host 'sicl-clos::compute-class-precedence-list-default r)
    (export-to-host 'sicl-clos::finalize-inheritance-default r)
    (export-to-host 'sicl-clos::effective-slot-definition-class r)
    (ld "../CLOS/class-finalization-defmethods.lisp" c r)))
