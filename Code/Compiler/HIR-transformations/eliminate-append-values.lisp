(cl:in-package #:sicl-hir-transformations)

(defun make-initial-instructions
    (successor
     values-location
     value-count-location
     cell-location
     dynamic-environment-location)
  (make-instance 'cleavir-ir:compute-return-value-count-instruction
    :output value-count-location
    :dynamic-environment-location dynamic-environment-location
    :successor
    (make-instance 'cleavir-ir:car-instruction
      :input values-location
      :output cell-location
      :successor successor)))

