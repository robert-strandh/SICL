(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:return-instruction)
     lexical-environment)
  (throw 'return
    (apply #'values *global-values-location*)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:funcall-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (input-values (loop for input in inputs
                             collect (input-value input lexical-environment))))
    (setf *global-values-location*
          (multiple-value-list (apply #'funcall input-values))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:assignment-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction))))
    (setf (gethash output lexical-environment)
          (input-value input lexical-environment)))
  (first (cleavir-ir:successors instruction)))
