(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction
    (client (instruction cleavir-ir:initialize-values-instruction) code-object)
  (let ((values-location-output
          (first (cleavir-ir:outputs instruction))))
    (setf (cleavir-ir:outputs instruction) '())
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:return-value-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value 0)
       :output values-location-output)
     instruction)))
