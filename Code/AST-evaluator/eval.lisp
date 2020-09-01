(cl:in-package #:sicl-ast-evaluator)

(defun eval (client environment cst)
  (funcall (compile nil (translate-code client environment cst))
           environment))

(defmethod cleavir-cst-to-ast:cst-eval ((client client:sicl) cst environment)
  (eval client environment cst))

(defmethod cleavir-cst-to-ast:eval ((client client:sicl) form environment)
  (let ((cst (cst:cst-from-expression form)))
    (eval client environment cst)))
