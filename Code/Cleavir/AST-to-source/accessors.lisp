(cl:in-package #:cleavir-ast-to-source)

(defmethod to-source ((ast cleavir-ast:car-ast) dictionary)
  `(cleavir-low:car
    ,(to-source (cleavir-ast:cons-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:cdr-ast) dictionary)
  `(cleavir-low:cdr
    ,(to-source (cleavir-ast:cons-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:rplaca-ast) dictionary)
  `(cleavir-low:rplaca
    ,(to-source (cleavir-ast:cons-ast ast) dictionary)
    ,(to-source (cleavir-ast:object-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:rplacd-ast) dictionary)
  `(cleavir-low:rplacd
    ,(to-source (cleavir-ast:cons-ast ast) dictionary)
    ,(to-source (cleavir-ast:object-ast ast) dictionary)))
