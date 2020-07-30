(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:initialize-closure-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (closure (input-value (first inputs) lexical-environment))
         (static-environment-values
           (loop for input in (rest inputs)
                 collect (input-value input lexical-environment))))
    (apply (aref (lexical-value 'static-environment lexical-environment)
                 sicl-compiler:+initialize-closure-function-index+)
           closure
           static-environment-values))
  (first (cleavir-ir:successors instruction)))
