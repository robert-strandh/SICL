(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:bind-instruction)
     lexical-environment)
  (destructuring-bind (function-input symbol-input value-input)
      (cleavir-ir:inputs instruction)
    (declare (ignore function-input))
    (setf (lexical-value (cleavir-ir:dynamic-environment-output instruction)
                         lexical-environment)
          (cons (make-instance 'sicl-run-time:special-variable-entry
                  :name (input-value symbol-input lexical-environment)
                  :value (input-value value-input lexical-environment))
                (lexical-value (cleavir-ir:dynamic-environment-location instruction)
                               lexical-environment))))
  (first (cleavir-ir:successors instruction)))
