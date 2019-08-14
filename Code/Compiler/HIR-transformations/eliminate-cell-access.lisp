(cl:in-package #:sicl-hir-transformations)

(defun eliminate-read-cell-instruction (read-cell-instruction)
  (change-class read-cell-instruction 'car-instruction))

(defun eliminate-read-cell-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (and (typep instruction 'cleavir-ir:read-cell-instruction)
                (not (eq owner top-level-enter-instruction)))
       (eliminate-read-cell-instructions instruction)))
   top-level-enter-instruction))

(defun eliminate-write-cell-instruction (write-cell-instruction)
  (change-class write-cell-instruction 'rplaca-instruction))

(defun eliminate-write-cell-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (and (typep instruction 'cleavir-ir:write-cell-instruction)
                (not (eq owner top-level-enter-instruction)))
       (eliminate-write-cell-instructions instruction)))
   top-level-enter-instruction))
