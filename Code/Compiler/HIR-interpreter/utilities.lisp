(cl:in-package #:sicl-hir-interpreter)

(defun lexical-value (name lexical-environment)
  (multiple-value-bind (value present-p)
      (gethash name lexical-environment)
    (assert present-p)
    value))

(defun (setf lexical-value) (value name lexical-environment)
  (setf (gethash name lexical-environment) value))

(defun input-value (input lexical-environment)
  (if (typep input 'cleavir-ir:constant-input)
      (cleavir-ir:value input)
      (lexical-value input lexical-environment)))

(defgeneric interpret-instruction (client instruction lexical-environment))

(defun interpret-instructions (client initial-instruction lexical-environment)
  (loop for instruction = initial-instruction
          then (interpret-instruction client instruction lexical-environment)))
