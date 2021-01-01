(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:save-values-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :outputs 1)
    (setf (output 0)
          (cons *global-values-location* dynamic-environment))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:restore-values-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment)
    (setf *global-values-location*
          (first dynamic-environment))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:initialize-values-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :outputs 1)
    (setf (output 0) '())
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:append-values-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :outputs 1)
    (setf (output 0)
          (append (output 0) *global-values-location*))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:multiple-value-call-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 3)
    (input 0) ; Avoid an unused variable warning.
    (setf *global-values-location*
          (multiple-value-list
           (apply (input 1) (input 2))))
    (successor 0)))
