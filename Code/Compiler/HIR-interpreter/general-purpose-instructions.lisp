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
    (let ((sicl-run-time:*dynamic-environment*
            (lexical-value (cleavir-ir:dynamic-environment-location instruction)
                           lexical-environment)))
      (setf *global-values-location*
            (multiple-value-list
             (let ((*call-stack*
                     (cons (make-instance 'call-stack-entry
                             :origin (cleavir-ast-to-hir:origin instruction)
                             :arguments input-values)
                           *call-stack*)))
               (apply #'funcall input-values))))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:assignment-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction))))
    (setf (lexical-value output lexical-environment)
          (input-value input lexical-environment)))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:eq-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (successors (cleavir-ir:successors instruction)))
    (if (eq (input-value (first inputs) lexical-environment)
            (input-value (second inputs) lexical-environment))
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:consp-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (successors (cleavir-ir:successors instruction)))
    (if (consp (input-value input lexical-environment))
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnump-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (successors (cleavir-ir:successors instruction)))
    (if (typep (input-value input lexical-environment) 'fixnum)
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:single-float-p-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (successors (cleavir-ir:successors instruction)))
    (if (typep (input-value input lexical-environment) 'single-float)
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:standard-object-p-instruction)
     lexical-environment)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (successors (cleavir-ir:successors instruction))
         (value (input-value input lexical-environment)))
    (if (or (characterp value)
            (typep value 'fixnum)
            (typep value 'single-float)
            (consp value))
        (second successors)
        (first successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:nop-instruction)
     lexical-environment)
  (first (cleavir-ir:successors instruction)))
