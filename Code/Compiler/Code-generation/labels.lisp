(cl:in-package #:sicl-code-generation)

(defun create-instruction-labels (instructions)
  (loop with table = (make-hash-table :test #'eq)
        for (instruction next) on instructions
        do (when (typep instruction 'cleavir-ir:catch-instruction)
             (loop for successor in (rest (cleavir-ir:successors instruction))
                   do (setf (gethash successor table)
                            (make-instance 'cluster:label))))
           (when (or (typep next 'cleavir-ir:enter-instruction)
                     (and (not (null next))
                          (let ((predecessors (cleavir-ir:predecessors next)))
                            (or (> (length predecessors) 1)
                                (not (eq instruction (first predecessors)))))))
             (setf (gethash next table)
                   (make-instance 'cluster:label)))
        finally (return table)))
                                
                             
