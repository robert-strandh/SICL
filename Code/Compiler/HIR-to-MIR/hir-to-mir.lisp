(cl:in-package #:sicl-hir-to-mir)

(defun process-instructions (client initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (process-instruction client instruction))
   initial-instruction))
