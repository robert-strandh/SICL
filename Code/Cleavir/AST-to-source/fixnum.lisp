(cl:in-package #:cleavir-ast-to-source)

(defmethod to-source ((ast cleavir-ast:fixnum-+-ast) dictionary)
  `(cleavir-low:fixnum-+ ,(to-source (cleavir-ast:arg1-ast ast))
			 ,(to-source (cleavir-ast:arg2-ast ast))))

(defmethod to-source ((ast cleavir-ast:fixnum---ast) dictionary)
  `(cleavir-low:fixnum-- ,(to-source (cleavir-ast:arg1-ast ast))
			 ,(to-source (cleavir-ast:arg2-ast ast))))

(defmethod to-source ((ast cleavir-ast:fixnum-<-ast) dictionary)
  `(cleavir-low:fixnum-< ,(to-source (cleavir-ast:arg1-ast ast))
			 ,(to-source (cleavir-ast:arg2-ast ast))))

(defmethod to-source ((ast cleavir-ast:fixnum-<=-ast) dictionary)
  `(cleavir-low:fixnum-<= ,(to-source (cleavir-ast:arg1-ast ast))
			  ,(to-source (cleavir-ast:arg2-ast ast))))

