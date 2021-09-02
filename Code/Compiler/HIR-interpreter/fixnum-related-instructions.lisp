(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-add-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (successors (cleavir-ir:successors instruction)))
    (setf (lexical-value output lexical-environment)
          (+ (input-value (first inputs) lexical-environment)
             (input-value (second inputs) lexical-environment)))
    ;; Assume the result is always a fixnum.
    (first successors)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-sub-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (successors (cleavir-ir:successors instruction)))
    (setf (lexical-value output lexical-environment)
          (- (input-value (first inputs) lexical-environment)
             (input-value (second inputs) lexical-environment)))
    ;; Assume the result is always a fixnum.
    (first successors)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-equal-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (successors (cleavir-ir:successors instruction)))
    (if (= (input-value (first inputs) lexical-environment)
           (input-value (second inputs) lexical-environment))
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-less-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (successors (cleavir-ir:successors instruction)))
    (if (< (input-value (first inputs) lexical-environment)
           (input-value (second inputs) lexical-environment))
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-not-greater-instruction)
     lexical-environment)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (successors (cleavir-ir:successors instruction)))
    (if (<= (input-value (first inputs) lexical-environment)
            (input-value (second inputs) lexical-environment))
        (first successors)
        (second successors))))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-divide-instruction)
     lexical-environment)
  (destructuring-bind (dividend-input divisor-input)
      (cleavir-ir:inputs instruction)
    (destructuring-bind (quotient-location remainder-location)
        (cleavir-ir:outputs instruction)
      (multiple-value-bind (quotient remainder)
          (floor(input-value dividend-input lexical-environment)
                (input-value divisor-input lexical-environment))
        (setf (lexical-value quotient-location lexical-environment) quotient)
        (setf (lexical-value remainder-location lexical-environment) remainder))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-logand-instruction)
     lexical-environment)
  (destructuring-bind (arg1-input arg2-input)
      (cleavir-ir:inputs instruction)
    (setf (lexical-value (first (cleavir-ir:outputs instruction))
                         lexical-environment)
          (logand (input-value arg1-input lexical-environment)
                  (input-value arg2-input lexical-environment))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-logior-instruction)
     lexical-environment)
  (destructuring-bind (arg1-input arg2-input)
      (cleavir-ir:inputs instruction)
    (setf (lexical-value (first (cleavir-ir:outputs instruction))
                         lexical-environment)
          (logior (input-value arg1-input lexical-environment)
                  (input-value arg2-input lexical-environment))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-logxor-instruction)
     lexical-environment)
  (destructuring-bind (arg1-input arg2-input)
      (cleavir-ir:inputs instruction)
    (setf (lexical-value (first (cleavir-ir:outputs instruction))
                         lexical-environment)
          (logxor (input-value arg1-input lexical-environment)
                  (input-value arg2-input lexical-environment))))
  (first (cleavir-ir:successors instruction)))

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:fixnum-lognot-instruction)
     lexical-environment)
  (setf (lexical-value (first (cleavir-ir:outputs instruction))
                       lexical-environment)
        (lognot (input-value (first (cleavir-ir:inputs instruction))
                             lexical-environment)))
  (first (cleavir-ir:successors instruction)))
