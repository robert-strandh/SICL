(cl:in-package #:cleavir-ast-to-source)

(defmethod to-source ((ast cleavir-ast:fixnum-add-ast) dictionary)
  `(cleavir-primop:fixnum-+
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)
    ,(to-source (cleavir-ast:variable-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-sub-ast) dictionary)
  `(cleavir-primop:fixnum--
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)
    ,(to-source (cleavir-ast:variable-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-less-ast) dictionary)
  `(cleavir-primop:fixnum-less
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-not-greater-ast) dictionary)
  `(cleavir-primop:fixnum-not-greater
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-greater-ast) dictionary)
  `(cleavir-primop:fixnum-greater
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-not-less-ast) dictionary)
  `(cleavir-primop:fixnum-not-less
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

