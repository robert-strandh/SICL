(cl:in-package #:sicl-boot-phase1)

(defun ld (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   filename (compilation-environment environment) environment))

(defun fill-environment (environment)
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
  (ld "temporary-ensure-class.lisp" environment)
  (ld "../../CLOS/standard-object-defclass.lisp" environment)
  (ld "../../CLOS/metaobject-defclass.lisp" environment)
  (ld "../../CLOS/method-defclass.lisp" environment)
  (ld "../../CLOS/standard-method-defclass.lisp" environment)
  (ld "../../CLOS/standard-accessor-method-defclass.lisp" environment)
  (ld "../../CLOS/standard-reader-method-defclass.lisp" environment)
  (ld "../../CLOS/standard-writer-method-defclass.lisp" environment)
  (ld "../../CLOS/slot-definition-defclass.lisp" environment)
  (ld "../../CLOS/standard-slot-definition-defclass.lisp" environment)
  (ld "../../CLOS/specializer-defclass.lisp" environment)
  (ld "../../CLOS/eql-specializer-defclass.lisp" environment)
  (ld "../../CLOS/class-unique-number-defparameter.lisp" environment)
  (ld "../../CLOS/class-defclass.lisp" environment)
  (ld "../../CLOS/forward-referenced-class-defclass.lisp" environment)
  (ld "../../CLOS/real-class-defclass.lisp" environment)
  (ld "../../CLOS/regular-class-defclass.lisp" environment)
  (ld "../../CLOS/standard-class-defclass.lisp" environment)
  (ld "../../CLOS/funcallable-standard-class-defclass.lisp" environment)
  (ld "../../CLOS/built-in-class-defclass.lisp" environment)
  (ld "../../Environment/standard-environment-functions.lisp" environment))
