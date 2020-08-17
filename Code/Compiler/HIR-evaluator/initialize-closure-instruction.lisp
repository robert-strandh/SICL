(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:initialize-closure-instruction)
     lexical-environment)
  (let* ((static-environment-index (value-index 'static-environment lexical-environment))
         (inputs (cleavir-ir:inputs instruction))
         (closure-index (value-index (first inputs) lexical-environment))
         (input-indices
           (loop for input in (rest inputs)
                 collect (value-index input lexical-environment))))
    (make-thunk (client instruction lexical-environment)
      (apply (aref (lref static-environment-index)
                   sicl-compiler:+initialize-closure-function-index+)
             (lref closure-index)
             (loop for input-index in input-indices
                   collect (lref input-index)))
      (successor 0))))
