(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:enclose-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (enter (cleavir-ir:code instruction)))
    (setf (lexical-value output lexical-environment)
          (apply (aref (lexical-value 'static-environment lexical-environment)
                       sicl-compiler:+enclose-function-index+)
                 (hir-to-host-function client enter)
                 (aref (lexical-value 'static-environment lexical-environment)
                       sicl-compiler:+code-object-index+)
                 (loop for input in inputs
                       collect (input-value input lexical-environment)))))
  (first (cleavir-ir:successors instruction)))
