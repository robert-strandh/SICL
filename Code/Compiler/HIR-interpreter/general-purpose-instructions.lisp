(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:return-instruction)
     lexical-environment)
  (throw 'return
    (apply #'values *global-values-location*)))
