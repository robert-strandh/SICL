(cl:in-package #:sicl-hir-to-cl)

(defmethod translate
    (client (instruction cleavir-ir:compute-argument-count-instruction) context)
  `((setq ,(cleavir-ir:name (first (cleavir-ir:outputs instruction)))
          (length ,*arguments-variable*))))

(defmethod translate
    (client (instruction cleavir-ir:argument-instruction) context)
  `((setq ,(cleavir-ir:name (first (cleavir-ir:outputs instruction)))
          (elt ,*arguments-variable*
               ,(translate-input (first (cleavir-ir:inputs instruction)))))))
