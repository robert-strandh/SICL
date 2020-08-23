(cl:in-package #:sicl-ast-evaluator)

(defclass client () ())

(defun translate-code (code environment)
  (let* ((cst (cst:cst-from-expression code))
         (client (make-instance 'client))
         (ast (cleavir-cst-to-ast:cst-to-ast
               client cst environment)))
    (translate-ast ast environment '())))
