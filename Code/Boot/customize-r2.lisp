(cl:in-package #:sicl-boot)

(defun define-validate-superclass (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass (r2 boot))
	(constantly t)))

;;; The problem that we are solving here is that during class
;;; initialization, there is a test that each superclass is of type
;;; CLASS, and that test uses TYPEP like this (TYPEP c 'CLASS).  But
;;; if we use the host version of TYPEP, it will return NIL because a
;;; bridge class is not a host class.  We solve this problem by
;;; supplying a slightly modified version of TYPEP in R2.  This
;;; modified version calls the host TYPEP in all cases except when the
;;; second argument is the symbol CLASS.  In that case, it instead
;;; supplies a different second argument to the host TYPEP.  This
;;; second argument is the name that the class named CLASS is know to
;;; by the host.  We can find this name by applying CLASS-NAME to the
;;; metaobject that we obtain by calling FIND-CLASS in R1 with the
;;; symbol CLASS.
(defun define-typep (boot)
  (setf (sicl-genv:fdefinition 'typep (r2 boot))
	(lambda (object type)
	  (typep object
		 (if (eq type 'class)
		     (class-name (sicl-genv:find-class 'class (r1 boot)))
		     type)))))

;;; We define a special version of ENSURE-GENERIC-FUNCTION in the
;;; run-time environment R2.  This version checks whether there is
;;; already a function named FUNCTION-NAME in R2.  If so that function
;;; is returned, and it is assumed to be a generic function.  If not,
;;; an instance of the host class STANDARD-GENERIC-FUNCTION is created
;;; and associated with FUNCTION-NAME in R2.
(defun define-ensure-generic-function-r2 (boot)
  (setf (sicl-genv:fdefinition 'ensure-generic-function (r2 boot))
	(lambda (function-name &rest arguments)
	  (let ((args (copy-list arguments)))
	    (loop while (remf args :environment))
	    (if (sicl-genv:fboundp function-name (r2 boot))
		(sicl-genv:fdefinition function-name (r2 boot))
		(setf (sicl-genv:fdefinition function-name (r2 boot))
		      (apply #'make-instance 'standard-generic-function
			     :name function-name
			     args)))))))

(defun define-default-superclasses (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses (r2 boot))
	(lambda (class)
	  (cond ((eq (class-of class)
		     (sicl-genv:find-class 'standard-class (r1 boot)))
		 (sicl-genv:find-class 'standard-object (r2 boot)))
		((eq (class-of class)
		     (sicl-genv:find-class 'sicl-clos:funcallable-standard-class
					   (r1 boot)))
		 (sicl-genv:find-class 'sicl-clos:funcallable-standard-object
				       (r2 boot)))
		(t
		 '())))))

(defun define-reader-method-class (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class (r2 boot))
	(lambda (&rest arguments)
	  (declare (ignore arguments))
	  (sicl-genv:find-class 'sicl-clos:standard-reader-method (r1 boot)))))

(defun define-writer-method-class (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class (r2 boot))
	(lambda (&rest arguments)
	  (declare (ignore arguments))
	  (sicl-genv:find-class 'sicl-clos:standard-writer-method (r1 boot)))))

(defun define-add-method (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:add-method (r2 boot))
	(lambda (generic-function method)
	  (push method (sicl-clos:generic-function-methods generic-function)))))

(defun customize-r2 (boot)
  (let ((c (c1 boot))
	(r (r2 boot)))
    (message "Customizing run-time environment R2~%")
    (define-make-instance boot)
    (define-direct-slot-definition-class boot)
    (define-find-class boot)
    (define-validate-superclass boot)
    (define-typep boot)
    (define-ensure-generic-function-r2 boot)
    (define-default-superclasses boot)
    (define-reader-method-class boot)
    (define-writer-method-class boot)
    (define-add-method boot)
    (ld "../CLOS/ensure-generic-function-using-class-support.lisp" c r)
    (message "Finished customizing run-time environment R2~%")))

;;  LocalWords:  metaobject
