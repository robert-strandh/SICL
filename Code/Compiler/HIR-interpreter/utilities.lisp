(cl:in-package #:sicl-hir-interpreter)

(defun input-value (input lexical-environment)
  (if (typep input 'cleavir-ir:constant-input)
      (cleavir-ir:value input)
      (gethash input lexical-environment)))

(defgeneric interpret-instruction (client instruction lexical-environment))

(defun interpret-instructions (client initial-instruction lexical-environment)
  (loop for instruction = initial-instruction
          then (interpret-instruction client instruction lexical-environment)))
