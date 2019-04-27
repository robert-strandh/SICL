(cl:in-package #:sicl-hir-to-cl)

(defun hir-to-cl (initial-instruction)
  (let ((*visited* (make-hash-table :test #'eq)))
    (translate-enter-instruction initial-instruction)))
