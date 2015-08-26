(cl:in-package #:cleavir-remove-useless-instructions)

(defparameter *remove-useless-instructions-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "REMOVE-USELESS-INSTRUCTIONS-METER"))
