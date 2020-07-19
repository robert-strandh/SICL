(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:aref-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (output 0)
          (row-major-aref (input 0) (input 1)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:aset-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 3 :outputs 0)
    (setf (row-major-aref (input 0) (input 1)) (input 2))
    (successor 0)))
