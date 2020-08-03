(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:initialize-closure-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (static-environment-cell (value-cell 'static-environment lexical-environment))
         (closure-cell (value-cell (first inputs) lexical-environment))
         (static-environment-cells
           (loop for input in (rest inputs)
                 collect (value-cell input lexical-environment))))
    (make-thunk (client instruction lexical-environment)
      (apply (aref (car static-environment-cell)
                   sicl-compiler:+initialize-closure-function-index+)
             (car closure-cell)
             (mapcar #'car static-environment-cells))
      (successor 0))))
