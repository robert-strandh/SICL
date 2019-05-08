(cl:in-package #:sicl-hir-to-cl)

(defmethod translate (client (instruction cleavir-ir:car-instruction) context)
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
  `((setf ,(cleavir-ir:name output)
          (car ,(cleavir-ir:name input))))))

(defmethod translate (client (instruction cleavir-ir:cdr-instruction) context)
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    `((setf ,(cleavir-ir:name output)
            (cdr ,(cleavir-ir:name input))))))

(defmethod translate (client (instruction cleavir-ir:rplaca-instruction) context)
  (let ((inputs (cleavir-ir:inputs instruction)))
    `((rplaca ,(cleavir-ir:name (first inputs))
              ,(cleavir-ir:name (second inputs))))))

(defmethod translate (client (instruction cleavir-ir:rplacd-instruction) context)
  (let ((inputs (cleavir-ir:inputs instruction)))
    `((rplacd ,(cleavir-ir:name (first inputs))
              ,(cleavir-ir:name (second inputs))))))
