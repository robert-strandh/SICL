(cl:in-package #:cleavir-hir-transformations)

(defun hir-transformations (initial-instruction implementation processor os)
  (type-inference initial-instruction)
  (eliminate-typeq initial-instruction)
  (introduce-immediates initial-instruction implementation processor os)
  (process-captured-variables initial-instruction)
  (process-fdefinitions initial-instruction implementation processor os))
