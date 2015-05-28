(cl:in-package #:sicl-boot)

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

