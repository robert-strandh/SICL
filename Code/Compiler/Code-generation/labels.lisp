(cl:in-package #:sicl-code-generation)

(defun instructions-needing-label (instructions)
  (loop with table = (make-hash-table :test #'eq)
        for (instruction next) on instructions
        do (when (or (typep next 'cleavir-ir:enter-instruction)
                     (and (not (null next))
                          (let ((predecessors (cleavir-ir:predecessors next)))
                            (or (> (length predecessors) 1)
                                (not (eq instruction (first predecessors)))))))
             (setf (gethash next table) t))
        finally (return table)))
                                
                             
