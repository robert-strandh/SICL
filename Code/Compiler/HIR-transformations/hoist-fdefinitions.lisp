(cl:in-package #:sicl-hir-transformations)

(defclass find-function-cell-instruction (cleavir-ir:instruction)
  ((%name :initarg :name :reader name)))

(defun transform-fdefinition-instruction
    (top-level-enter-instruction fdefinition-instruction table)
  (let ((input (first (cleavir-ir:inputs fdefinition-instruction))))
    (if (typep input 'cleavir-ir:lexical-location)
        (let* ((assignment-instruction (first (cleavir-ir:defining-instructions input)))
               (constant-input (first (cleavir-ir:inputs assignment-instruction)))
               (function-name (cleavir-ir:value constant-input))
               (new (make-instance 'find-function-cell-instruction
                      :name function-name
                      :inputs '()
                      :output input)))
          (change-class assignment-instruction 'cleavir-ir:nop-instruction
                        :inputs '()
                        :outputs '())
          (change-class fdefinition-instruction 'cleavir-ir:car-instruction)
          (cleavir-ir:insert-instruction-after new top-level-enter-instruction))
        (let* ((function-name (cleavir-ir:value input)))
          (if (null (gethash function-name table))
              (let* ((temp (cleavir-ast-to-hir:make-temp))
                     (new (make-instance 'find-function-cell-instruction
                            :name function-name
                            :inputs '()
                            :output temp)))
                (change-class fdefinition-instruction 'cleavir-ir:car-instruction
                              :inputs (list temp))
                (cleavir-ir:insert-instruction-after new top-level-enter-instruction)
                (setf (gethash function-name table) temp))
              (change-class fdefinition-instruction 'cleavir-ir:car-instruction
                            :inputs (list (gethash function-name table))))))))

(defun find-fdefinition-instructions (top-level-enter-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:fdefinition-instruction)
         (push instruction result)))
     top-level-enter-instruction)
    result))

(defun hoist-fdefinitions (top-level-enter-instruction)
  (let ((fdefinition-instructions
          (find-fdefinition-instructions top-level-enter-instruction))
        (table (make-hash-table :test #'equal)))
    (loop for fdefinition-instruction in fdefinition-instructions
          do (transform-fdefinition-instruction
              top-level-enter-instruction fdefinition-instruction table))))
