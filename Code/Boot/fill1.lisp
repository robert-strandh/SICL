(cl:in-package #:sicl-boot)

(defun define-ensure-generic-function (c r)
  (setf (sicl-genv:fdefinition 'ensure-generic-function r)
	(let ((ensure-generic-function  (sicl-genv:fdefinition
					 'ensure-generic-function
					 c)))
	  (lambda (function-name &rest arguments)
	    (if (sicl-genv:fboundp function-name r)
		(sicl-genv:fdefinition function-name r)
		(let ((new-arguments (copy-list arguments)))
		  (loop while (remf new-arguments :environment))
		  (setf (sicl-genv:fdefinition function-name r)
			(apply ensure-generic-function
			       (gensym)
			       new-arguments))))))))

(defun define-make-instance (c r)
  (setf (sicl-genv:fdefinition 'make-instance r)
	(let ((make-instance (sicl-genv:fdefinition
			      'make-instance
			      c)))
	  (lambda (&rest arguments)
	    (if (symbolp (first arguments))
		(apply make-instance
		       (sicl-genv:find-class
			(first arguments)
			r)
		       (rest arguments))
		(apply make-instance arguments))))))

(defun define-class-prototype (r)
  (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype r)
	#'closer-mop:class-prototype))

(defun define-generic-function-method-class (r)
  (setf (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class
			       r)
	#'closer-mop:generic-function-method-class))

(defun fill1 (boot)
  (let ((c (c1 boot))
	(r (r1 boot)))
    (define-ensure-generic-function c r)
    (define-make-instance c r)
    (define-class-prototype r)
    (define-generic-function-method-class r)))
