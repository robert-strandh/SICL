(cl:in-package #:sicl-hir-to-mir)

(defgeneric hir-to-mir (client initial-instruction))

(defun replace-instruction (client instruction)
  (let ((replacement (translate client instruction)))
    (unless (eq replacement instruction)
      (loop for predecessor in (cleavir-ir:predecessors instruction)
            do (nsubstitute replacement instruction
                            (cleavir-ir:successors predecessor))))))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64) initial-instruction)
  (let ((*translation-table* (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction) (replace-instruction client instruction))
     initial-instruction)))
