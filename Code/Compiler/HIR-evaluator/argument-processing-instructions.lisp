(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:compute-argument-count-instruction)
     lexical-environment)
  (let ((arguments-cell (value-cell 'arguments lexical-environment)))
    (make-thunk (client instruction lexical-environment :outputs 1)
      (setf (output 0)
            (length (car arguments-cell)))
      (successor 0))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:argument-instruction)
     lexical-environment)
  (let ((arguments-cell (value-cell 'arguments lexical-environment)))
    (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
      (setf (output 0)
            (aref (car arguments-cell) (input 0)))
      (successor 0))))
