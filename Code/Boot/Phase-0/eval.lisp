(cl:in-package #:sicl-boot-phase-0)

(defmethod cleavir-cst-to-ast:cst-eval ((client sicl-boot:client) cst environment)
  (sicl-hir-evaluator:cst-eval client cst environment))

(defmethod cleavir-cst-to-ast:eval ((client sicl-boot:client) form environment)
  (let ((cst (cst:cst-from-expression form)))
    (sicl-hir-evaluator:cst-eval client cst environment)))
