(cl:in-package #:sicl-hir-to-mir)

(defun process-instructions (client top-level-enter-instruction)
  (cleavir-ir:map-instructions-with-owner
   (lambda (instruction owner)
     (unless (eq owner top-level-enter-instruction)
       (process-instruction client instruction)))
   top-level-enter-instruction))

(defun hir-to-mir (client top-level-enter-instruction)
  (process-enclose-instructions top-level-enter-instruction)
  (process-create-cell-instruction top-level-enter-instruction)
  (process-fetch top-level-enter-instruction)
  (process-instructions client top-level-enter-instruction))
