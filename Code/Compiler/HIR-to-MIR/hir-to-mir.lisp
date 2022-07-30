(cl:in-package #:sicl-hir-to-mir)

(defun convert-one-function (client enter-instruction)
  (let ((instructions '()))
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (push instruction instructions))
     enter-instruction)
    (loop for instruction in instructions
          do (process-instruction client instruction))))

(defun hir-to-mir (client code-object)
  (let ((ir (sicl-compiler:ir code-object))
        (*literals* (sicl-compiler:literals code-object)))
    (expand-funcall-instructions ir)
    (loop with enter-instructions = (gather-enter-instructions ir)
          for enter-instruction in enter-instructions
          collect (convert-one-function client enter-instruction))
    ir))
