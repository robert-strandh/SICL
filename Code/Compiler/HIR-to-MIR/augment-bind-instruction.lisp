(cl:in-package #:sicl-hir-to-mir)

(defmethod process-instruction
    (client (instruction cleavir-ir:bind-instruction) code-object)
  (let ((dynamic-environment-output
          (first (cleavir-ir:outputs instruction))))
    (setf (cleavir-ir:outputs instruction) '())
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:return-value-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value 0)
       :output dynamic-environment-output)
     instruction)))
