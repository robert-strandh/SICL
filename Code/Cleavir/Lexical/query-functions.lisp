(cl:in-package #:cleavir-lexical)


(defgeneric describe-variable (client environment name))

(defgeneric describe-function (client environment name))

(defgeneric describe-block (client environment name))

(defgeneric describe-tag (client environment tag))

(defgeneric describe-optimize (client environment))
