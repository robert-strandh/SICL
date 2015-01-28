(cl:in-package #:sicl-extrinsic-environment)

(defun add-special-operators (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	when (special-operator-p symbol)
	  do (setf (sicl-env:special-operator symbol environment) t)))

(defun add-nil-and-t (environment)
  (setf (sicl-env:constant-variable t environment) t)
  (setf (sicl-env:constant-variable nil environment) nil))

(defun define-defmacro (environment)
  (setf (sicl-env:macro-function 'defmacro environment)
	(compile nil
		 (cleavir-code-utilities:parse-macro
		  'defmacro
		  '(name lambda-list &body body)
		  `((eval-when (:compile-toplevel :load-toplevel :execute)
		      (setf (sicl-env:macro-function name environment)
			    (compile nil
				     (cleavir-code-utilities:parse-macro
				      name
				      lambda-list
				      body)))))))))

(defun add-default-setf-expander (environment)
  (setf (sicl-env:default-setf-expander environment)
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
			`(,(first form) ,@temps)))))))

(defun fill-environment (environment)
  (add-special-operators environment)
  (add-nil-and-t environment)
  (define-defmacro environment)
  (add-default-setf-expander environment))
