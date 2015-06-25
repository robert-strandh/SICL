(cl:in-package #:sicl-boot)

;;; Define the macro DEFGENERIC in compile-time environment C1.  We
;;; define it a bit differently from its usual definition.  Its main
;;; purpose is to define a generic function in the run-time
;;; environment R1.  However, before definining it, we remove the
;;; existing generic function if it exists.  This way, we are sure to
;;; get a fresh generic function, as opposed to one that happened to
;;; have been imported from the host.  We must, of course, make sure
;;; that we execute a DEFGENERIC form for a particular generic
;;; function exactly once, but we can do that because we completely
;;; master the boot process.  We also put the same definition in the
;;; compile-time environment C1 so that the compiler sees it when
;;; subsequent forms that use this function are compiled.
(defun define-defgeneric-c1 (boot)
  (setf (sicl-genv:macro-function 'defgeneric (c1 boot))
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(second form) ,(r1 boot))
		  (setf (sicl-genv:fdefinition ',(second form) ,(r1 boot))
			(ensure-generic-function
			 ',(second form)
			 :name ',(second form)
			 :lambda-list ',(third form)))
		  (setf (sicl-genv:fdefinition ',(second form) ,(c1 boot))
			(sicl-genv:fdefinition ',(second form) ,(r1 boot)))))))

;;; The purpose of this function is to redefine the macro DEFGENERIC
;;; in the compilation environment C2.  The new definition is only
;;; used to create bridge-class accessors in run-time environment
;;; during phase 2.  Therefore, we can process only the name and the
;;; lambda-list of each DEFGENERIC form.  Furthermore, by passing all
;;; the initialization arguments to MAKE-INSTANCE, we avoid having to
;;; implement the generic-function initialization protocol in phase 2.
(defun define-defgeneric-c2 (boot)
  (setf (sicl-genv:macro-function 'defgeneric (c2 boot))
	(lambda (form environment)
	  (declare (ignore environment))
	  `(let* ((r1 ,(r1 boot))
		  (class-name 'standard-generic-function)
		  (class (sicl-genv:find-class class-name r1))
		  (method-class-name 'standard-method)
		  (method-class (sicl-genv:find-class method-class-name r1))
		  (gf (make-instance class
			:name ',(second form)
			:lambda-list ',(third form)
			:argument-precedence-order ',(third form)
			:declarations '()
			:documentation nil
			;; FIXME: supply a method-combination metaobject.
			:method-combination nil
			:method-class method-class)))
	     (setf (sicl-genv:fdefinition ',(second form) ,(r3 boot)) gf)))))

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
;;; run-time environment R1.  This version checks whether there is
;;; already a function named FUNCTION-NAME in R1.  If so that function
;;; is returned, and it is assumed to be a generic function.  If not,
;;; an instance of the host class STANDARD-GENERIC-FUNCTION is created
;;; and associated with FUNCTION-NAME in R1.
(defun define-ensure-generic-function-r1 (boot)
  (setf (sicl-genv:fdefinition 'ensure-generic-function (r1 boot))
	(lambda (function-name &rest arguments)
	  (let ((args (copy-list arguments)))
	    (loop while (remf args :environment))
	    (if (sicl-genv:fboundp function-name (r1 boot))
		(sicl-genv:fdefinition function-name (r1 boot))
		(setf (sicl-genv:fdefinition function-name (r1 boot))
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

(defun define-accessors (slot-spec class boot)
  (let ((slot-name (getf slot-spec :name)))
    (loop for reader in (getf slot-spec :readers)
	  for function = (compile nil `(lambda (args next-methods)
					 (declare (ignore next-methods))
					 (slot-value (car args) ',slot-name)))
	  for method = (make-instance 'standard-method
			 :lambda-list '(object)
			 :specializers (list class)
			 :function function)
	  do (add-method (sicl-genv:fdefinition reader (r1 boot)) method))
    (loop for writer in (getf slot-spec :writers)
	  for function = (compile nil `(lambda (args next-methods)
					 (declare (ignore next-methods))
					 (setf (slot-value (cadr args) ',slot-name)
					       (car args))))
	  for method = (make-instance 'standard-method
			 :lambda-list '(new-value object)
			 :specializers (list (find-class t) class)
			 :function function)
	  do (add-method (sicl-genv:fdefinition writer (r1 boot)) method))))

(defun define-ensure-class-r1 (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class (r1 boot))
	(lambda (class-name
		 &key
		   direct-slots
		   ((:direct-superclasses direct-superclass-names))
		   name
		   ((:metaclass metaclass-name) 'standard-class)
		 &allow-other-keys)
	  (message "Creating class ~s~%" class-name)
	  (let ((metaclass (find-class metaclass-name))
		(slot-copies
		  (loop for slot-spec in direct-slots
			collect (loop for (name value) on slot-spec by #'cddr
				      unless (member name '(:readers :writers))
					collect name
					and collect value)))
		(direct-superclasses
		  (loop for name in direct-superclass-names
			collect (sicl-genv:find-class name (r1 boot)))))
	    (let ((class (make-instance metaclass
			   :name (make-symbol (symbol-name name))
			   :direct-slots slot-copies
			   :direct-superclasses direct-superclasses)))
	      (setf (sicl-genv:find-class class-name (r1 boot)) class)
	      (loop for slot-spec in direct-slots
		    do (define-accessors slot-spec class boot)))))))

(defun define-class-t-r1 (boot)
  (setf (sicl-genv:find-class 't (r1 boot))
	(find-class 'standard-object)))

(defun define-class-function-r1 (boot)
  (setf (sicl-genv:find-class 'function (r1 boot))
	(make-instance 'closer-mop:funcallable-standard-class
	  :name (make-symbol (symbol-name '#:function)))))

(defun define-funcallable-standard-class ()
  (setf (find-class 'sicl-clos:funcallable-standard-class)
	(find-class 'closer-mop:funcallable-standard-class)))

(defun customize-environments (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot))
	(r2 (r2 boot)))
    (message "Customizing environments~%")
    (define-defgeneric-c1 boot)
    (define-defgeneric-c2 boot)
    (define-class-t-r1 boot)
    (define-class-function-r1 boot)
    (define-funcallable-standard-class)
    (ld "../CLOS/make-method-lambda-support.lisp" c1 c1)
    (ld "../CLOS/make-method-lambda-defuns.lisp" c1 c1)
    (ld "../CLOS/ensure-method.lisp" c1 r1)
    (define-generic-function-method-class c1)
    (define-generic-function-method-class r1)
    (define-ensure-class-r1 boot)
    (define-make-instance boot)
    (define-direct-slot-definition-class boot)
    (define-find-class boot)
    (define-validate-superclass boot)
    (define-typep boot)
    (define-ensure-generic-function-r1 boot)
    (define-default-superclasses boot)
    (define-reader-method-class boot)
    (define-writer-method-class boot)
    (define-add-method boot)
    (ld "../CLOS/ensure-generic-function-using-class-support.lisp" c1 r2)
    (message "Finished customizing environments~%")))
