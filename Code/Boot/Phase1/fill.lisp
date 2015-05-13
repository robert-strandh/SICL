(cl:in-package #:sicl-boot-phase1)

(defun ld (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   filename (compilation-environment environment) environment))

(defun fill-environment (environment)
  (setf (sicl-genv:find-class 'standard-class
			      (compilation-environment environment))
	(find-class 'temporary-standard-class))
  (sicl-genv:fmakunbound 'sicl-clos:ensure-generic-function-using-class
			 environment)
  (setf (sicl-genv:fdefinition 'make-instance environment)
	(let ((make-instance (sicl-genv:fdefinition
			      'make-instance
			      (compilation-environment environment))))
	  (lambda (&rest arguments)
	    (if (symbolp (first arguments))
		(apply make-instance
		       (sicl-genv:find-class
			(first arguments)
			(compilation-environment environment))
		       (rest arguments))
		(apply make-instance arguments)))))
  (ld "../../CLOS/ensure-class-using-class-support.lisp"
      environment)
  (ld "../../Environment/standard-environment-functions.lisp" environment))
