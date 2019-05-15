(cl:in-package #:sicl-hir-to-cl)

(defmethod translate (client (instruction cleavir-ir:box-instruction) context)
  (let* ((input (translate-input (first (cleavir-ir:inputs instruction))))
         (output (cleavir-ir:name (first (cleavir-ir:outputs instruction)))))
    `((setq ,output ,input))))

(defmethod translate (client (instruction cleavir-ir:unbox-instruction) context)
  (let* ((input (translate-input (first (cleavir-ir:inputs instruction))))
         (output (cleavir-ir:name (first (cleavir-ir:outputs instruction)))))
    `((setq ,output ,input))))
