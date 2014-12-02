(cl:in-package #:sicl-extrinsic-hir-compiler)

(setf (sicl-env:macro-function 'defmacro *environment*)
      (compile nil
	       (cleavir-code-utilities:parse-macro
		'defmacro
		'(name lambda-list &body body)
		`((eval-when (:compile-toplevel :load-toplevel :execute)
		    (setf (sicl-env:macro-function name *environment*)
			  (compile nil
				   (cleavir-code-utilities:parse-macro
				    name
				    lambda-list
				    body))))))))

(setf (sicl-env:macro-function 'in-package *environment*)
      (lambda (form environment)
	(declare (ignore environment))
	(setq *package* (find-package (cadr form)))
	`(setq *package* (find-package ',(cadr form)))))

(setf (sicl-env:default-setf-expander *environment*)
      (lambda (form)
	(if (symbolp form)
	    (let ((new (gensym)))
	      (values '()
		      '()
		      `(,new)
		      `(setq ,form ,new)
		      form))
	    (let ((temps (loop for arg in (rest form) collect (gensym)))
		  (new (gensym)))
	      (values temps
		      (rest form)
		      `(,new)
		      `(funcall #'(setf ,(first form)) ,new ,@temps)
		      `(,(first form) ,@temps))))))

;;; Defined FBOUNDP in the extrinsic environment.
(setf (sicl-env:fdefinition 'fboundp *environment*)
      (lambda (name)
	(sicl-env:fboundp name *environment*)))

;;; We need to be able to add new functions to the environment, so we
;;; need a definition of (SETF FDEFINITION).
(setf (sicl-env:fdefinition '(setf fdefinition) *environment*)
      (lambda (&rest args)
	(unless (= (length args) 2)
	  (funcall (sicl-env:fdefinition 'cl:error *environment*)
		   "wrong number of arguments"))
	(destructuring-bind (new-function name) args
	  (unless (functionp new-function)
	    (funcall (sicl-env:fdefinition 'cl:error *environment*)
		   "Argument to (SETF FDEFINITION) must be a function ~s"
		   new-function))
	  (unless (or (symbolp name)
		      (and (consp name)
			   (consp (cdr name))
			   (null (cddr name))
			   (eq (car name) 'setf)
			   (symbolp (cadr name))))
	    (funcall (sicl-env:fdefinition 'cl:error *environment*)
		     "(SETF FDEFINITION) must be given a function name, not ~s"
		     name))
	  (setf (sicl-env:fdefinition name *environment*)
		new-function))))

;;; We also need the function FUNCALL, because that is what is used by
;;; the default SETF expander.  At the momement, it only handles
;;; functions as its first argument.
(setf (sicl-env:fdefinition 'funcall *environment*)
      (lambda (&rest args)
	(unless (plusp (length args))
	  (funcall (sicl-env:fdefinition 'cl:error *environment*)
		   "wrong number of arguments"))
	(unless (functionp (first args))
	  (funcall (sicl-env:fdefinition 'cl:error *environment*)
		   "First argument to must be a function ~s"
		   (first args)))
	(apply (first args) (rest args))))

;;; We need a definition of the function VALUES, and the host one is
;;; just fine for this.
(setf (sicl-env:fdefinition 'values *environment*)
      #'values)

;;; Function SYMBOL-VALUE.  It searches the runtime stack to see
;;; whether there is a binding for the variable.  If no binding is
;;; found, it uses the variable-cell in the global environment.
;;;
;;; FIXME: Check argument count etc.
(setf (sicl-env:fdefinition 'symbol-value *environment*)
      (let ((env *environment*))
	(lambda (symbol)
	  (loop with unbound = (sicl-env:variable-unbound symbol env)
		with cell = (sicl-env:variable-cell symbol env)
		with error = (sicl-env:fdefinition 'cl:error env)
		for entry in *dynamic-environment*
		do (when (and (typep entry 'variable-binding)
			      (eq (symbol entry) symbol))
		     (if (eq (value entry) unbound)
			 (funcall error "unbound variable ~s" symbol)
			 (return (value entry))))
		finally
		   (if (eq (car cell) unbound)
		       (funcall error "unbound variable ~s" symbol)
		       (return (car cell)))))))

;;; Function (SETF SYMBOL-VALUE).  It searches the runtime stack to
;;; see whether there is a binding for the variable.  If no binding is
;;; found, it uses the variable-cell in the global environment.
(setf (sicl-env:fdefinition '(setf symbol-value) *environment*)
      (let ((env *environment*))
	(lambda (value symbol)
	  (loop with cell = (sicl-env:variable-cell symbol env)
		for entry in *dynamic-environment*
		do (when (and (typep entry 'variable-binding)
			      (eq (symbol entry) symbol))
		     (setf (value entry) value)
		     (return value))
		finally
		   (setf (car cell) value)
		   (return value)))))

;;; Set the variable SICL-ENV:*ENVIRONMENT* in the environment.
(setf (sicl-env:special-variable 'sicl-env:*global-environment* *environment* t)
      *environment*)

;;; This definition allows us to find the definition of any host function.
;;; It is not ideal right now because it can fail and call ERROR.
(setf (sicl-env:fdefinition 'host-fdefinition *environment*)
      #'fdefinition)
