(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; We need to start using DEFMACRO early on to define macros, and
;;; since we don't already have it, we must create it "manually".
;;; This version is incorrect, though, because it uses the host
;;; compiler both to create the macro function for DEFMACRO (which is
;;; fine) and for creating the macro function for the macros defined
;;; by DEFMACRO (which is not fine).  As a result, the macros defined
;;; by this version of DEFMACRO must be defined in the NULL lexical
;;; environment.  Luckily, most macros are, and certainly the ones we
;;; need to define with this version of DEFMACRO until we can replace
;;; it with a native version.
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

;;; Fill the target environment with all available packages in the host.
(setf (sicl-env:packages *environment*)
      (list-all-packages))
