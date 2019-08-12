(cl:in-package #:sicl-hir-to-mir)

(defun process-read-cell-instruction (read-cell-instruction)
  (change-class read-cell-instruction 'car-instruction))

(defun process-read-cell-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (and (typep instruction 'cleavir-ir:read-cell-instruction)
                (not (eq owner top-level-enter-instruction)))
       (process-read-cell-instructions instruction)))
   top-level-enter-instruction))

(defun process-write-cell-instruction (write-cell-instruction)
  (change-class write-cell-instruction 'rplaca-instruction))

(defun process-write-cell-instructions (top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (when (and (typep instruction 'cleavir-ir:write-cell-instruction)
                (not (eq owner top-level-enter-instruction)))
       (process-write-cell-instructions instruction)))
   top-level-enter-instruction))
