(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:unwind-instruction)
     lexical-environment)
  (let* ((unwind-index (cleavir-ir:unwind-index instruction))
         (destination (cleavir-ir:destination instruction))
         (successors (cleavir-ir:successors destination))
         (successor #'dummy-successor))
    (prog1 (make-thunk (client instruction lexical-environment :inputs 2 :successors 0)
             (throw (input 1) successor))
      (setf successor
            (instruction-thunk client (nth unwind-index successors) lexical-environment)))))
