(cl:in-package #:sicl-data-and-control-flow)

(defmacro define-setf-expander (&environment env name lambda-list &body body)
  `(let ((expander ,(cleavir-code-utilities:parse-macro name lambda-list body))
	 (global-env ,(sicl-genv:global-environment env)))
     (setf (sicl-global-environment:setf-expander ',name global-env)
	   expander)))
