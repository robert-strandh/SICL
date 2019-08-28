(cl:in-package #:sicl-hir-to-mir)

(defun hir-to-mir (client top-level-enter-instruction)
  (eliminate-enclose-instructions client top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (unless (eq owner top-level-enter-instruction)
       (process-instruction client instruction)))
   top-level-enter-instruction))
