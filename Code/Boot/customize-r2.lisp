(cl:in-package #:sicl-boot)

(defun define-validate-superclass (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass (r2 boot))
	(constantly t)))

(defun customize-r2 (boot)
  (let ((c (c1 boot))
	(r (r2 boot)))
    (define-make-instance boot)
    (define-direct-slot-definition-class boot)
    (define-find-class boot)
    (define-validate-superclass boot)
    (ld "../CLOS/ensure-generic-function-using-class-support.lisp" c r)))
