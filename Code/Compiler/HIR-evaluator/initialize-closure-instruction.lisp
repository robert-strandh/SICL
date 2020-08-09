(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:initialize-closure-instruction)
     lexical-environment)
  (let* ((static-environment-cell (value-cell 'static-environment lexical-environment))
         (inputs (cleavir-ir:inputs instruction))
         (closure-cell (value-cell (first inputs) lexical-environment))
         (input-cells
           (loop for input in (rest inputs)
                 collect (value-cell input lexical-environment))))
    (make-thunk (client instruction lexical-environment)
      (apply (aref (car static-environment-cell) sicl-compiler:+initialize-closure-function-index+)
             (car closure-cell)
             (mapcar #'car input-cells))
      (successor 0))))
