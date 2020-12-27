(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:unwind-protect-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
      (setf (output 0)
            (cons (make-instance 'sicl-run-time:unwind-protect-entry
                    :thunk (input 0))
                  dynamic-environment))
      (successor 0)))
      
