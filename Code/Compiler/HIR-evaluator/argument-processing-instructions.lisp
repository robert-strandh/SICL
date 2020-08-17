(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:compute-argument-count-instruction)
     lexical-environment)
  (let ((arguments-index (value-index 'arguments lexical-environment)))
    (make-thunk (client instruction lexical-environment :outputs 1)
      (setf (output 0)
            (length (lref arguments-index)))
      (successor 0))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:argument-instruction)
     lexical-environment)
  (let ((arguments-index (value-index 'arguments lexical-environment)))
    (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
      (setf (output 0)
            (aref (lref arguments-index) (input 0)))
      (successor 0))))
