(cl:in-package #:cleavir-mir-interpreter)

(defmethod execute-instruction
    ((instruction cleavir-mir:car-instruction) environment)
  (let* ((input (car (cleavir-mir:inputs instruction)))
	 (output (car (cleavir-mir:outputs instruction)))
	 (input-value (read-value input environment)))
    (assert (consp input-value))
    (write-value output environment (car input-value))))



