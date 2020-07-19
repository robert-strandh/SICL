(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:bind-instruction)
     lexical-environment)
  (let ((output-cell
          (value-cell
           (cleavir-ir:dynamic-environment-output instruction)
           lexical-environment)))
    (make-thunk (client instruction lexical-environment :inputs 3)
      (setf (car output-cell)
            (cons (make-instance 'sicl-run-time:special-variable-entry
                    :name (input 1)
                    :value (input 2))
                  dynamic-environment))
      (successor 0))))
