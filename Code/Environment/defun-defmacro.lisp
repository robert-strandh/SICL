(cl:in-package #:sicl-global-environment)

(defmacro defun (&environment env name lambda-list &body body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    (let ((global-env (cleavir-env:global-environment env))
	  (arg-type
	    (cleavir-code-utilities:lambda-list-type-specifier lambda-list)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (setf (sicl-env:function-type ',name ,global-env)
		 `(function ,',arg-type t)))
	 (eval-when (:load-toplevel :execute)
	   (setf (sicl-env:fdefinition ',name ,global-env)
		 (lambda ,lambda-list
		   ,@declarations
		   ,@(if (null documentation)
			 '()
			 (list documentation))
		   (block ,(if (symbolp name) name (second name))
		     ,@forms))))))))
