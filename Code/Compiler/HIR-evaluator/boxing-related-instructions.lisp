(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:box-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0) (input 0))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:unbox-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs  1)
    (setf (output 0) (input 0))
    (successor 0)))
