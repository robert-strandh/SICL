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

(defmethod to-source ((ast cleavir-ast:slot-read-ast) dictionary)
  `(cleavir-low:slot-read
    ,(to-source (cleavir-ast:slot-ast ast) dictionary)
    ,(to-source (cleavir-ast:slot-number-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:slot-write-ast) dictionary)
  `(cleavir-low:slot-read
    ,(to-source (cleavir-ast:slot-ast ast) dictionary)
    ,(to-source (cleavir-ast:slot-number-ast ast) dictionary)
    ,(to-source (cleavir-ast:value-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:aref-ast) dictionary)
  `(cleavir-low:aref
    ,(to-source (cleavir-ast:array ast) dictionary)
    ,(to-source (cleavir-ast:index-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:aset-ast) dictionary)
  `(cleavir-low:aset
    ,(to-source (cleavir-ast:array ast) dictionary)
    ,(to-source (cleavir-ast:index-ast ast) dictionary)
    ,(to-source (cleavir-ast:value-ast ast) dictionary)))

