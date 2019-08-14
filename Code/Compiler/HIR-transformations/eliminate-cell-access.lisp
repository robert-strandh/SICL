(cl:in-package #:sicl-hir-transformations)

(defun eliminate-read-cell-instruction (read-cell-instruction)
  (change-class read-cell-instruction 'cleavir-ir:car-instruction))

(defun eliminate-read-cell-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:read-cell-instruction)
       (eliminate-read-cell-instruction instruction)))
   top-level-enter-instruction))

(defun eliminate-write-cell-instruction (write-cell-instruction)
  (change-class write-cell-instruction 'cleavir-ir:rplaca-instruction))

(defun eliminate-write-cell-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (when (typep instruction 'cleavir-ir:write-cell-instruction)
       (eliminate-write-cell-instruction instruction)))
   top-level-enter-instruction))
