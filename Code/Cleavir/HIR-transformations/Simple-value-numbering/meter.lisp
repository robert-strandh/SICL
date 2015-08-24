(cl:in-package #:cleavir-simple-value-numbering)

(defparameter *simple-value-numbering-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "SIMPLE-VALUE-NUMBERING-METER"))
