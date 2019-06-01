(cl:in-package #:sicl-boot-phase-0)

(defmethod cleavir-cst-to-ast:cst-eval ((client client) cst environment)
  (sicl-hir-to-cl:cst-eval client cst environment))
