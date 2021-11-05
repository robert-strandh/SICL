(cl:in-package #:cleavir-ast-visualizer)

(defgeneric background-color (ast))

(defmethod background-color (ast)
  clim:+white+)

(defmethod background-color ((ast cleavir-ast:literal-ast))
  clim:+pink+)
