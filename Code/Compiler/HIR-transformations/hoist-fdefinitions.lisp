(cl:in-package #:sicl-hir-transformations)

(defclass find-function-cell-instruction (cleavir-ir:instruction)
  ((%name :initarg :name :reader name)))

(defun transform-fdefinition-instruction (top-level-enter-instruction
                                          fdefinition-instruction)
  (let* ((variable (first (cleavir-ir:inputs fdefinition-instruction)))
         (assignment-instruction (first (cleavir-ir:defining-instructions variable)))
         (constant-input (first (cleavir-ir:inputs assignment-instruction)))
         (function-name (cleavir-ir:value constant-input))
         (dynamic-environment-location
           (cleavir-ir:dynamic-environment-output top-level-enter-instruction))
         (new (make-instance 'find-function-cell-instruction
                :name function-name
                :inputs '()
                :output variable
                :dynamic-environment-location dynamic-environment-location)))
    (change-class assignment-instruction 'cleavir-ir:nop-instruction
                  :inputs '()
                  :outputs '())
    (change-class fdefinition-instruction 'cleavir-ir:car-instruction)
    (cleavir-ir:insert-instruction-after new top-level-enter-instruction)))

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
          (find-fdefinition-instructions top-level-enter-instruction)))
    (loop for fdefinition-instruction in fdefinition-instructions
          do (transform-fdefinition-instruction top-level-enter-instruction
                                                fdefinition-instruction))))
