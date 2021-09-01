(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:save-values-instruction)
     lexical-environment)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (setf (lexical-value output lexical-environment)
          (cons *global-values-location*
                (lexical-value (cleavir-ir:dynamic-environment-location instruction)
                               lexical-environment))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:restore-values-instruction)
     lexical-environment)
  (setf *global-values-location*
        (first (lexical-value (cleavir-ir:dynamic-environment-location instruction)
                              lexical-environment)))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:initialize-values-instruction)
     lexical-environment)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (setf (lexical-value output lexical-environment)
          '()))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:append-values-instruction)
     lexical-environment)
  (let ((output (first (cleavir-ir:outputs instruction))))
    (setf (lexical-value output lexical-environment)
          (append (lexical-value output lexical-environment)
                  *global-values-location*)))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:multiple-value-call-instruction)
     lexical-environment)
  (destructuring-bind (call-function-location function-location values-location)
      (cleavir-ir:inputs instruction)
    (declare (ignore call-function-location))
    (setf *global-values-location*
          (multiple-value-list
           (apply (input-value function-location lexical-environment)
                  (input-value values-location lexical-environment)))))
  (first (cleavir-ir:successors instruction)))
