(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:enclose-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (enter (cleavir-ir:code instruction)))
    (setf (gethash output lexical-environment)
          (apply #'enclose
                 (hir-to-host-function client enter)
                 (aref (gethash 'static-environment lexical-environment) 0)
                 (loop for input in inputs
                       collect (input-value input lexical-environment)))))
  (first (cleavir-ir:successors instruction)))
