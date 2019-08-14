(cl:in-package #:sicl-hir-to-cl)

(defmethod translate (client (instruction cleavir-ir:fetch-instruction) context)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (static-environment-input (first inputs))
         (static-environment-input-name (cleavir-ir:name static-environment-input))
         (index-input (second inputs))
         (index (cleavir-ir:value index-input))
         (output (first (cleavir-ir:outputs instruction)))
         (output-name (cleavir-ir:name output)))
    `((setq ,output-name
            (aref ,static-environment-input-name ,(+ index 1))))))

(defmethod translate (client (instruction cleavir-ir:create-cell-instruction) context)
  (let* ((output (first (cleavir-ir:outputs instruction)))
         (output-name (cleavir-ir:name output)))
    `((setq ,output-name (list nil)))))
