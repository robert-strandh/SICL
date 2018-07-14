(cl:in-package #:cleavir-load-time-value-hoisting-test)

(defclass test-system ()
  ((%compilation-environment
    :initform (make-instance 'sicl-extrinsic-environment:environment)
    :reader compilation-environment)))


