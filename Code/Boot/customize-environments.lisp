(cl:in-package #:sicl-boot)

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
	     (setf (sicl-genv:fdefinition ',(second form) ,(r2 boot)) gf)))))

(defun define-temporary-ensure-method-c1-r1 (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos::temporary-ensure-method (c1 boot))
	(lambda (function-name
		 lambda-list
		 qualifiers
		 specializers
		 documentation
		 function)
	  (format t "Calling temporary-ensure-method ~s ~s~%"
		  function-name specializers)
	  (let* ((fun (sicl-genv:fdefinition function-name (r1 boot)))
		 (specs (loop for s in specializers
			      collect
			      (if (eq s 't)
				  ;; When the specializer is T, we
				  ;; don't mean the class named T in
				  ;; R1, and instead we mean "not
				  ;; specialized", and for that to
				  ;; happen, we need to find the host
				  ;; class named T.
				  (find-class t)
				  (sicl-genv:find-class s (r1 boot)))))
		 (method (make-instance 'standard-method
			  :lambda-list lambda-list
			  :qualifiers qualifiers
			  :specializers specs
			  :documentation documentation
			  :function function)))
	    (add-method fun method)
	    method)))
  (setf (sicl-genv:fdefinition 'sicl-clos::temporary-ensure-method (r1 boot))
	(sicl-genv:fdefinition 'sicl-clos::temporary-ensure-method (c1 boot))))

(defun define-validate-superclass (boot)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass (r2 boot))
	(constantly t)))

;;; The problem that we are solving here is that during class
;;; initialization, there is a test that each superclass is of type
;;; CLASS, and that test uses TYPEP like this (TYPEP c 'CLASS).  But
;;; if we use the host version of TYPEP, it will return NIL because a
;;; bridge class is not a host class.  We solve this problem by
;;; supplying a slightly modified version of TYPEP in R2.  This
;;; modified version relies on the fact that when TYPEP is called this
;;; way, it is either called with a symbol or with a bridge class.  We
;;; capture this case, and return T when we are called with a standard
;;; object.
(defun define-typep (boot)
  (setf (sicl-genv:fdefinition 'typep (r2 boot))
	(lambda (object type)
	  (if (eq type 'class)
	      (typep object 'standard-object)
	      (typep object type)))))

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

;;; Recall that the function DEFAULT-SUPERCLASSES is a SICL-SPECIFIC
;;; function that is called by the class-initialization protocol to
;;; determine a list of default superclasses when no superclasses are
;;; given for the creation of a class.  This AMOP section:
;;; http://metamodular.com/CLOS-MOP/initialization-of-class-metaobjects2.html
;;; describes that when the class that is being instantiated is
;;; STANDARD-CLASS, then the default superclasses is a list of a
;;; single class, namely the class named STANDARD-OBJECT, and that
;;; when the class that is being instantiated is
;;; FUNCALLABLE-STANDARD-CLASS, then the default superclasses is a
;;; list of a single class, namely the class named
;;; FUNCALLABLE-STANDARD-OBJECT.  However, in SICL, we turned that
;;; rule into a generic function called DEFAULT-SUPERCLASSES that have
;;; methods specialized to STANDARD-CLASS, and
;;; FUNCALLABLE-STANDARD-CLASS, but other methods can be added as
;;; well.
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

;;; Recall that the function READER-METHOD-CLASS is called as part of
;;; the class-initialization protocol as described in this section
;;; http://metamodular.com/CLOS-MOP/initialization-of-class-metaobjects2.html
;;; of the AMOP.
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

(defun define-accessors (slot-spec class r)
  (let ((slot-name (getf slot-spec :name)))
    (loop for reader in (getf slot-spec :readers)
	  for function = (compile nil `(lambda (args next-methods)
					 (declare (ignore next-methods))
					 (slot-value (car args) ',slot-name)))
	  for method = (make-instance 'standard-method
			 :lambda-list '(object)
			 :specializers (list class)
			 :function function)
	  do (add-method (sicl-genv:fdefinition reader r) method))
    (loop for writer in (getf slot-spec :writers)
	  for function = (compile nil `(lambda (args next-methods)
					 (declare (ignore next-methods))
					 (setf (slot-value (cadr args) ',slot-name)
					       (car args))))
	  for method = (make-instance 'standard-method
			 :lambda-list '(new-value object)
			 :specializers (list (find-class t) class)
			 :function function)
	  do (add-method (sicl-genv:fdefinition writer r) method))))

(defun define-ensure-class (r)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class r)
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
			collect (sicl-genv:find-class name r))))
	    (let ((class (make-instance metaclass
			   :name (make-symbol (symbol-name name))
			   :direct-slots slot-copies
			   :direct-superclasses direct-superclasses)))
	      (setf (sicl-genv:find-class class-name r) class)
	      (loop for slot-spec in direct-slots
		    do (define-accessors slot-spec class r)))))))

;;; Since we do not use the host DEFCLASS macro nor the host version
;;; of ENSURE-CLASS in phase 1, our classes do not automatically have
;;; the host class named STANDARD-OBJECT as a superclass.  But being a
;;; subclass of STANDARD-OBJECT is a requirement for the host generic
;;; function INITIALIZE-INSTANCE to be able to initialize instances of
;;; a class.  We solve this problem by defining a special version of
;;; the class named T in phase 1 that in fact is the same as the host
;;; class STANDARD-OBJECT.  This way, we are sure that all our MOP
;;; classes in phase 1 are in fact subclass of the host class
;;; STANDARD-OBJECT.
(defun define-class-t-phase1 (environment)
  (setf (sicl-genv:find-class 't environment)
	(find-class 'standard-object)))

;;; We need a special definition of the class named FUNCTION in R1,
;;; because we want instances of this class to be funcallable in the
;;; host.  For that reason, we create this class as an instance of the
;;; host class FUNCALLABLE-STANDARD-CLASS.
(defun define-class-function-r1 (boot)
  (setf (sicl-genv:find-class 'function (r1 boot))
	(make-instance 'closer-mop:funcallable-standard-class
	  :name (make-symbol (symbol-name '#:function)))))

;;; This function defines class SICL-CLOS:FUNCALLABLE-STANDARD-CLASS
;;; in the host environment to be the same as the host version of that
;;; class.  It is needed because, although we import definitions in
;;; the host environment that are associated with the package
;;; CLOSER-MOP, in fact those symbols do not have CLOSER-MOP as their
;;; package.  Instead they are imported into the CLOSER-MOP package
;;; from the corresponding host-specific package.  We should eliminate
;;; the need for this function by not importing based on the package
;;; of the symbols, but based on whether the symbols are accessible in
;;; the CLOSER-MOP package.
(defun define-funcallable-standard-class ()
  (setf (find-class 'sicl-clos:funcallable-standard-class)
	(find-class 'closer-mop:funcallable-standard-class)))

(defun customize-for-phase1 (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot)))
    (message "Customizing environments for phase 1~%")
    (define-temporary-ensure-method-c1-r1 boot)
    (define-class-t-phase1 r1)
    (define-class-function-r1 boot)
    (define-funcallable-standard-class)
    ;; Rather than calling MAKE-METHOD-LAMBDA, the temporary
    ;; definition of the macro DEFMETHOD calls
    ;; MAKE-METHOD-LAMBDA-DEFAULT directly.
    (ld "defmethod-defmacro-c1.lisp" c1 c1)
    ;; Load a definition of MAKE-METHOD-LAMBDA-DEFAULT to be used with
    ;; the temporary definition of the macro DEFMETHOD.
    (ld "../CLOS/make-method-lambda-support.lisp" c1 c1)
    (ld "../CLOS/ensure-method.lisp" c1 r1)
    (define-ensure-class r1)
    (message "Finished customizing environments for phase 1~%")))

(defun customize-for-phase2 (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot))
	(r2 (r2 boot)))
    (message "Customizing environments for phase 2~%")
    (define-make-instance boot)
    (define-defgeneric-c2 boot)
    (define-direct-slot-definition-class boot)
    (define-find-class boot)
    (define-validate-superclass boot)
    (define-typep boot)
    (define-ensure-generic-function-r1 boot)
    (define-default-superclasses boot)
    (define-reader-method-class boot)
    (define-writer-method-class boot)
    (define-ensure-class r2)
    (define-add-method boot)
    (ld "../CLOS/invalidate-discriminating-function.lisp" c1 r1)
    (ld "../CLOS/generic-function-initialization-defmethods.lisp" c1 r1)
    (ld "../CLOS/ensure-generic-function-using-class-support.lisp" c1 r2)
    (message "Finished customizing environments for phase 2~%")))

(defun customize-environments (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot))
	(r2 (r2 boot)))
    (message "Customizing environments~%")
    (customize-for-phase1 boot)
    ;; (customize-for-phase2 boot)
    (message "Finished customizing environments~%")))
