(cl:in-package #:sicl-boot)

(defmethod cleavir-literals:allocate-lexical-location
    ((client client) environment)
  (cleavir-ast:make-ast 'cleavir-ast:lexical-ast :name (gensym)))

(defmethod cleavir-literals:convert-initialization-form
    ((client client) form environment)
  (cleavir-cst-to-ast:convert client form environment))

(defmethod cleavir-literals:convert-creation-form
    ((client client) form lexical-ast environment)
  (cleavir-ast:make-ast 'cleavir-ast:lexical-bind-ast
    :value-ast (cleavir-cst-to-ast:convert client form environment)
    :lexical-variable-ast lexical-ast))

(defmethod cleavir-cst-to-ast:trivial-constant-p
    ((client client) object)
  (typep object '(integer #.(- (expt 2 62)) #.(1- (expt 2 62)))))
