(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:car-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0)
          (car (input 0)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:cdr-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0)
          (cdr (input 0)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:rplaca-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2)
    (rplaca (input 0) (input 1))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:rplacd-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2)
    (rplacd (input 0) (input 1))
    (successor 0)))
