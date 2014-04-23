(cl:in-package #:cleavir-ast-to-source)

(defmethod to-source ((ast cleavir-ast:fixnum-arithmetic-ast) dictionary)
  `(cleavir-primop:fixnum-arithmetic 
    ,(to-source (cleavir-ast:variable-ast ast) dictionary)
    ,(to-source (cleavir-ast:operation-ast ast) dictionary)
    ,(to-source (cleavir-ast:normal-ast ast) dictionary)
    ,(to-source (cleavir-ast:overflow-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-+-ast) dictionary)
  `(cleavir-primop:fixnum-+
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)
    ,(to-source (cleavir-ast:variable-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum---ast) dictionary)
  `(cleavir-primop:fixnum--
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)
    ,(to-source (cleavir-ast:variable-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-<-ast) dictionary)
  `(cleavir-primop:fixnum-<
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum-<=-ast) dictionary)
  `(cleavir-primop:fixnum-<=
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum->-ast) dictionary)
  `(cleavir-primop:fixnum->
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

(defmethod to-source ((ast cleavir-ast:fixnum->=-ast) dictionary)
  `(cleavir-primop:fixnum->=
    ,(to-source (cleavir-ast:arg1-ast ast) dictionary)
    ,(to-source (cleavir-ast:arg2-ast ast) dictionary)))

