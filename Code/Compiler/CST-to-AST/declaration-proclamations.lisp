(cl:in-package #:sicl-cst-to-ast)

(defmethod cleavir-cst-to-ast:declaration-proclamations
    ((client sicl-client:sicl) environment)
  (env:proclamation client environment 'declaration))
