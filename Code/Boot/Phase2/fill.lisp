(cl:in-package #:sicl-boot-phase2)

(defun ld (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   (asdf:system-relative-pathname :sicl-boot-phase2 filename)
   (sicl-boot-phase1:compilation-environment (phase1-environment environment))
   environment))

(defun define-ensure-generic-function (environment)
  (setf (sicl-genv:fdefinition 'ensure-generic-function environment)
	(lambda (function-name &rest arguments)
	  (if (sicl-genv:fboundp function-name environment)
	      (sicl-genv:fdefinition function-name environment)
	      (let ((new-arguments (copy-list arguments)))
		(loop while (remf new-arguments :environment))
		(setf (sicl-genv:fdefinition function-name environment)
		      (apply #'make-instance
			     (sicl-genv:find-class
			      'standard-generic-function
			      (phase1-environment environment))
			     new-arguments)))))))

(defun fill-environment (environment)
  (define-ensure-generic-function environment)
  (sicl-genv:fmakunbound 'sicl-clos:method-function environment)
  (ld "../../CLOS/method-function-defgeneric.lisp" environment)
  (sicl-genv:fmakunbound 'sicl-clos:method-generic-function environment)
  (ld "../../CLOS/method-generic-function-defgeneric.lisp" environment)
  (sicl-genv:fmakunbound 'sicl-clos:method-lambda-list environment)
  (ld "../../CLOS/method-lambda-list-defgeneric.lisp" environment)
  (sicl-genv:fmakunbound 'sicl-clos:method-qualifiers environment)
  (ld "../../CLOS/method-qualifiers-defgeneric.lisp" environment)
  (sicl-genv:fmakunbound 'sicl-clos:class-direct-superclasses environment)
  (ld "../../CLOS/class-direct-superclasses-defgeneric.lisp" environment))
