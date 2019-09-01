(cl:in-package #:sicl-hir-interpreter)

(defun input-value (input lexical-environment)
  (if (typep input 'cleavir-ir:constant-input)
      (cleavir-ir:value input)
      (gethash input lexical-environment)))
