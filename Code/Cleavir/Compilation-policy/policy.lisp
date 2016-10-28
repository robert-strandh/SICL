(cl:in-package #:cleavir-compilation-policy)

(defclass policy ()
  ((%infer-types-p
    :initform t
    :initarg :infer-types-p
    :reader infer-types-p)))
