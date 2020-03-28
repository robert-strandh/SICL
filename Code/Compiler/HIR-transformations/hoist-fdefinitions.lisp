(cl:in-package #:sicl-hir-transformations)

(defun insert-find-function-cell (function-name output successor)
  (let ((zero (make-instance 'cleavir-ir:constant-input :value 0))
        (function-name-input
          (make-instance 'cleavir-ir:constant-input
            :value function-name))
        (find-function-cell-function-location
          (make-instance 'cleavir-ir:lexical-location
            :name (gensym "find-function-cell-function"))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:argument-instruction
       :input zero
       :output find-function-cell-function-location)
     successor)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:funcall-instruction
       :inputs (list find-function-cell-function-location function-name-input))
     successor)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:multiple-to-fixed-instruction
       :output output)
     successor)))

(defun transform-fdefinition-instruction
    (top-level-enter-instruction fdefinition-instruction table)
  (let ((input (first (cleavir-ir:inputs fdefinition-instruction)))
        (successor (first (cleavir-ir:successors top-level-enter-instruction))))
    (assert (typep input 'cleavir-ir:constant-input))
    (let ((function-name (cleavir-ir:value input)))
      (if (null (gethash function-name table))
          (let ((temp (cleavir-ast-to-hir:make-temp)))
            (insert-find-function-cell function-name temp successor)
            (change-class fdefinition-instruction 'cleavir-ir:car-instruction
                          :inputs (list temp))
            (setf (gethash function-name table) temp))
          (change-class fdefinition-instruction 'cleavir-ir:car-instruction
                        :inputs (list (gethash function-name table)))))))

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
