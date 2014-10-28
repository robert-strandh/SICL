(cl:in-package #:sicl-extrinsic-environment)

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

		    
