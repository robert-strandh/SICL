(cl:in-package #:sicl-hir-transformations)

(defun convert-one-symbol-value (instruction)
  (let ((variable-name-input (first (cleavir-ir:inputs instruction)))
        (function-temp (cleavir-ast-to-hir:make-temp))
        (function-name-input
          (make-instance 'cleavir-ir:constant-input
            :value 'symbol-value)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:fdefinition-instruction
       :input function-name-input
       :output function-temp
       :dynamic-environment-location
       (cleavir-ir:dynamic-environment-location instruction))
     instruction)
    (change-class instruction 'cleavir-ir:funcall-instruction
                  :inputs (list function-temp variable-name-input))))

(defun convert-symbol-value (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:symbol-value-instruction)
       (convert-one-symbol-value instruction)))
   initial-instruction))
