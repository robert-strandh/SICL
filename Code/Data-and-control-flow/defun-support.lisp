(cl:in-package #:sicl-data-and-control-flow)

(defun defun-expander (environment name lambda-list body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((global-env (sicl-genv:global-environment environment))
	  (arg-type
	    (cleavir-code-utilities:lambda-list-type-specifier lambda-list)))
      `(progn
         (eval-when (:compile-toplevel)
	   (setf (sicl-env:function-type ',name ,global-env)
		 `(function ,',arg-type t))
           (setf (sicl-genv:function-lambda-list ',name ,global-env)
                 ',lambda-list))
	 (eval-when (:load-toplevel :execute)
	   (setf (sicl-env:fdefinition ',name (sicl-genv:global-environment))
		 (lambda ,lambda-list
		   ,@declarations
		   ,@(if (null documentation)
			 '()
			 (list documentation))
		   (block ,(if (symbolp name) name (second name))
		     ,@forms)))
           (setf (sicl-env:function-type ',name (sicl-genv:global-environment))
                 `(function ,',arg-type t))
           (setf (sicl-genv:function-lambda-list ',name (sicl-genv:global-environment))
                 ',lambda-list)
           ',name)))))
