(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:compute-return-value-count-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :outputs 1)
    (setf (output 0)
          (length *global-values-location*))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:return-value-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0)
          (elt (the list *global-values-location*) (input 0)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:initialize-return-values-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1)
    (setf *global-values-location*
          (make-list (input 0)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:set-return-value-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2)
    (setf (elt (the list *global-values-location*) (input 0))
          (input 1))
    (successor 0)))
