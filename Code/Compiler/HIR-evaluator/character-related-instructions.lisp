(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:characterp-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :successors 2)
    (if (characterp (input 0))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:char-code-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0)
          (char-code (input 0)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:code-char-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0)
          (code-char (input 0)))
    (successor 0)))
