(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:unwind-instruction)
     lexical-environment)
  (destructuring-bind (function-input continuation-input)
      (cleavir-ir:inputs instruction)
    (declare (ignore function-input))
    (let* ((input-value (input-value continuation-input lexical-environment))
           (unwind-index (cleavir-ir:unwind-index instruction))
           (destination (cleavir-ir:destination instruction))
           (successors (cleavir-ir:successors destination))
           (successor (nth unwind-index successors)))
      (throw input-value successor))))
