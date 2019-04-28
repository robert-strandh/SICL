(cl:in-package #:sicl-hir-to-cl)

(defun hir-to-cl (initial-instruction)
  (let ((context (make-instance 'context)))
    (translate-enter-instruction initial-instruction context)))
