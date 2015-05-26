(cl:in-package #:sicl-boot-phase1)

(defun ld (filename environment)
  (format *trace-output* "Loading file ~a~%" filename)
  (finish-output *trace-output*)
  (sicl-extrinsic-environment:load-source-with-environments
   (asdf:system-relative-pathname :sicl-boot-phase1 filename)
   (compilation-environment environment)
   environment))

(defun define-ensure-generic-function (environment)
  (setf (sicl-genv:fdefinition 'ensure-generic-function environment)
	(let ((ensure-generic-function  (sicl-genv:fdefinition
					 'ensure-generic-function
					 (compilation-environment environment))))
	  (lambda (function-name &rest arguments)
	    (if (sicl-genv:fboundp function-name environment)
		(sicl-genv:fdefinition function-name environment)
		(let ((new-arguments (copy-list arguments)))
		  (loop while (remf new-arguments :environment))
		  (setf (sicl-genv:fdefinition function-name environment)
			(apply ensure-generic-function
			       (gensym)
			       new-arguments))))))))

(defun define-make-instance (environment)
  (setf (sicl-genv:fdefinition 'make-instance environment)
	(let ((make-instance (sicl-genv:fdefinition
			      'make-instance
			      (compilation-environment environment))))
	  (lambda (&rest arguments)
	    (if (symbolp (first arguments))
		(apply make-instance
		       (sicl-genv:find-class
			(first arguments)
			environment)
		       (rest arguments))
		(apply make-instance arguments))))))

(defun define-class-prototype (environment)
  (setf (sicl-genv:fdefinition 'sicl-clos:class-prototype environment)
	#'closer-mop:class-prototype))

(defun define-generic-function-method-class (environment)
  (setf (sicl-genv:fdefinition 'sicl-clos:generic-function-method-class
			       environment)
	#'closer-mop:generic-function-method-class))

(defun fill-environment (environment)
  (define-ensure-generic-function environment)
  (define-make-instance environment)
  (define-class-prototype environment)
  (define-generic-function-method-class environment)
  (ld "../../CLOS/ensure-class-using-class-support.lisp" environment)
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
  (ld "../../CLOS/direct-slot-definition-defclass.lisp" environment)
  (ld "../../CLOS/effective-slot-definition-defclass.lisp" environment)
  (ld "../../CLOS/standard-direct-slot-definition-defclass.lisp" environment)
  (ld "../../CLOS/standard-effective-slot-definition-defclass.lisp" environment)
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
  (ld "function-temporary-defclass.lisp" environment)
  (ld "../../CLOS/funcallable-standard-object-defclass.lisp" environment)
  (ld "../../CLOS/generic-function-defclass.lisp" environment)
  (ld "../../CLOS/standard-generic-function-defclass.lisp" environment)
  ;;
  ;; Preparation for phase 2.  When we start making instances of
  ;; generic functions with classes defined in the phase 1
  ;; environment, we are going to want to access the slots of those
  ;; generic functions.  The accessors for those slots are generic
  ;; functions defined in the phase 1 environment, so the code that
  ;; calls those accessors should also be loaded in the phase 1
  ;; environment.
  ;;
  ;; Recall that the generic function classes defined here inherit
  ;; from the host class funcallable-standard-object.  In order to set
  ;; the funcallable instance function of the generic functions that
  ;; are instances of those generic function classes, we must call the
  ;; host version of SET-FUNCALLABLE-INSTANCE-FUNCTION.  We make that
  ;; possible by defining SICL-CLOS:SET-FUNCALLABLE-INSTANCE-FUNCTION
  ;; in this environment to be an alias for the analogous host
  ;; function.
  (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function
			       environment)
	#'closer-mop:set-funcallable-instance-function))

;;  LocalWords:  accessors funcallable
