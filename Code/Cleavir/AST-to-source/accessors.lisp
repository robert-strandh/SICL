(cl:in-package #:cleavir-ast-to-source)

(defmethod to-source ((ast cleavir-ast:car-ast) dictionary)
  `(cleavir-low:car ,(to-source (cleavir-ast:cons-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:cdr-ast) dictionary)
  `(cleavir-low:cdr ,(to-source (cleavir-ast:cons-ast ast) dictionary)))
