(cl:in-package #:sicl-hir-transformations)

(defun find-fdefinitions (top-level-enter-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:fdefinition-instruction)
         (push instruction result)))
     top-level-enter-instruction)
    result))

(defun hoist-fdefinitions (top-level-enter-instruction)
  (declare (ignore top-level-enter-instruction))
  nil)
