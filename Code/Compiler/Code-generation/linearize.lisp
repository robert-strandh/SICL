(cl:in-package #:sicl-code-generation)

(defun linearize-lir (enter-instruction)
  (let ((result '())
        (enter-instructions
          (sicl-hir-to-mir:gather-enter-instructions enter-instruction)))
    (loop for enter-instruction in enter-instructions
          do (loop with local-worklist = (list enter-instruction)
                   with visited = (make-hash-table :test #'eq)
                   until (null local-worklist)
                   do (let ((instruction (pop local-worklist)))
                        (unless (gethash instruction visited)
                          (setf (gethash instruction visited) t)
                          (push instruction result)
                          (loop for successor in (reverse (cleavir-ir:successors instruction))
                                do (push successor local-worklist))))))
    (reverse result)))
