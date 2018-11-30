(cl:in-package #:sicl-hir-to-mir)

(defgeneric hir-to-mir (client initial-instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64) initial-instruction)
  (let ((*translation-table* (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction) (translate client instruction))
     initial-instruction)))
