(cl:in-package #:sicl-hir-transformations)

(defun eliminate-create-cell-instruction (instruction)
  (let* ((nil-constant
           (make-instance 'cleavir-ir:constant-input :value 'nil)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:named-call-instruction
       :callee-name 'cons
       :inputs (list nil-constant nil-constant)
       :successor instruction)
     instruction)
    (change-class instruction 'cleavir-ir:return-value-instruction
                  :inputs (list (make-instance 'cleavir-ir:constant-input
                                  :value 0)))))

(defun eliminate-create-cell-instructions (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:create-cell-instruction)
       (eliminate-create-cell-instruction instruction)))
   initial-instruction))
