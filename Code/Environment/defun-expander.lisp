(cl:in-package #:sicl-global-environment)

(defun defun-expander (environment name lambda-list body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((global-env (cleavir-env:global-environment environment))
	  (arg-type
	    (cleavir-code-utilities:lambda-list-type-specifier lambda-list)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (setf (sicl-env:function-type ',name ,global-env)
		 `(function ,',arg-type t)))
	 (eval-when (:load-toplevel :execute)
	   (setf (sicl-env:fdefinition ',name (sicl-genv:global-environment))
		 (lambda ,lambda-list
		   ,@declarations
		   ,@(if (null documentation)
			 '()
			 (list documentation))
		   (block ,(if (symbolp name) name (second name))
		     ,@forms)))
	   ',name)))))
