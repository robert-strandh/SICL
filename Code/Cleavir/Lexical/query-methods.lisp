(cl:in-package #:cleavir-lexical)

(defmethod describe-variable (client (environment environment) name)
  (trucler:describe-variable client environment name))

(defmethod describe-function (client (environment environment) name)
  (trucler:describe-function client environment name))

(defmethod describe-block (client (environment environment) name)
  (trucler:describe-block client environment name))

(defmethod describe-tag (client (environment environment) tag)
  (trucler:describe-tag client environment tag))

(defmethod describe-optimize (client (environment environment))
  (trucler:describe-optimize client environment))
