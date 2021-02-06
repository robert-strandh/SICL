(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:bind-instruction)
     lexical-environment)
  (let ((output-index
          (ensure-lref
           (cleavir-ir:dynamic-environment-output instruction)
           lexical-environment)))
    (make-thunk (client instruction lexical-environment :inputs 2)
      (setf (lref output-index)
            (cons (make-instance 'sicl-run-time:special-variable-entry
                    :name (input 0)
                    :value (input 1))
                  dynamic-environment))
      (successor 0))))
