(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:multiple-to-fixed-instruction)
     lexical-environment)
  (let ((outputs (cleavir-ir:outputs instruction))
        (successor (first (cleavir-ir:successors instruction))))
    (loop for output in outputs
          do (setf (gethash output lexical-environment)
                   (pop *global-values-location*)))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixed-to-multiple-instruction)
     lexical-environment)
  (let ((inputs (cleavir-ir:inputs instruction))
        (successor (first (cleavir-ir:successors instruction))))
    (setf *global-values-location*
          (loop for input in inputs
                collect (input-value input lexical-environment)))
    successor))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:save-values-instruction)
     lexical-environment)
  (push *global-values-location* *values-environment*)
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:restore-values-instruction)
     lexical-environment)
  (setf *global-values-location* (pop *values-environment*))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:multiple-value-call-instruction)
     lexical-environment)
  (let ((input-value (input-value (first (cleavir-ir:inputs instruction))
                                  lexical-environment))
        (successor (first (cleavir-ir:successors instruction))))
    (setf *global-values-location*
          (multiple-value-list
           (apply input-value
                  (loop until (null *values-environment*)
                        for result = '()
                          then (append (pop *values-environment*) result)
                        finally (return result)))))
    successor))
