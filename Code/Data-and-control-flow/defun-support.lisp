(cl:in-package #:sicl-data-and-control-flow)

(defun defun-expander (name lambda-list body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((global-env-var (gensym))
	  (arg-type
	    (cleavir-code-utilities:lambda-list-type-specifier lambda-list)))
      `(progn
         (eval-when (:compile-toplevel)
           (let ((,global-env-var (sicl-genv:global-environment)))
             (setf (sicl-env:function-type ',name ,global-env-var)
                   `(function ,',arg-type t))
             (setf (sicl-genv:function-lambda-list ',name ,global-env-var)
                   ',lambda-list)))
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
