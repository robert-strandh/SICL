(cl:in-package #:sicl-hir-to-mir)

(defun convert-one-function (client enter-instruction)
  (eliminate-enclose-instructions client enter-instruction)
  (cleavir-ir:map-local-instructions
   (lambda (instruction)
     (process-instruction client instruction))
   enter-instruction))

(defun hir-to-mir (client top-level-enter-instruction)
  (let ((enter-instructions
          (gather-enter-instructions top-level-enter-instruction)))
    (loop for enter-instruction in enter-instructions
          collect (convert-one-function client enter-instruction))))
