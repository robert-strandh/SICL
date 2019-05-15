(cl:in-package #:sicl-hir-to-cl)

(defmethod translate (client (instruction cleavir-ir:aref-instruction) context)
  (declare (ignore context client))
  (let ((array-input (translate-input (first (cleavir-ir:inputs instruction))))
        (index-input (translate-input (second (cleavir-ir:inputs instruction))))
        (object-output (cleavir-ir:name (first (cleavir-ir:outputs instruction)))))
    `((setf ,object-output
            (row-major-aref ,array-input ,index-input)))))

(defmethod translate (client (instruction cleavir-ir:aset-instruction) context)
  (declare (ignore context client))
  (let ((array-input (translate-input (first (cleavir-ir:inputs instruction))))
        (index-input (translate-input (second (cleavir-ir:inputs instruction))))
        (object-input (translate-input (third (cleavir-ir:inputs instruction)))))
    `((setf (row-major-aref ,array-input ,index-input) ,object-input))))
