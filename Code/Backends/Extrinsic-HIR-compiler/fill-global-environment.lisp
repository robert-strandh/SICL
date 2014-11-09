(cl:in-package #:sicl-extrinsic-hir-compiler)

(loop for symbol being each external-symbol in '#:common-lisp
      when (special-operator-p symbol)
	do (setf (sicl-env:special-operator symbol *environment*) t))

(setf (sicl-env:constant-variable t *environment*) t)
(setf (sicl-env:constant-variable nil *environment*) nil)

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
		      `(funcall #'(setf ,(first form) ,new ,@temps))
		      `(,(first form) ,@temps))))))

;;; Many very basic functions will refer to CL:ERROR, so before we can
;;; start filling the environment with those basic functions, it is
;;; good to have a preliminary definition of CL:ERROR in the
;;; environment.  That preliminary definition can not do much at this
;;; point, so it just calls the host function named CL:ERROR in a way
;;; that can't fail.  This way of doing it at least lets us inspect
;;; the host stack and the target runtime environment. 
(setf (sicl-env:fdefinition 'cl:error *environment*)
      #'error)

;;; Import some simple functions to from the host to the target
;;; environment.
(defprimitive cl:consp (t))
(defprimitive cl:cons (t t))

(defprimitive cl:numberp (t))
(defprimitive cl:integerp (t))
(defprimitive cl:rationalp (t))

(defprimitive cl:null (t))

(defprimitive cl:symbolp (t))

(defprimitive cl:characterp (t))

(defprimitive cl:stringp (t))
