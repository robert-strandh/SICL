(cl:in-package #:cleavir-hir-transformations)

(defun hir-transformations
    (initial-instruction implementation &optional processor os)
  (declare (ignore processor os))
  (type-inference initial-instruction)
  (eliminate-load-time-value-inputs initial-instruction implementation)
  (eliminate-typeq initial-instruction)
  (process-captured-variables initial-instruction))
