(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-add-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (output 0)
          (+ (input 0) (input 1)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-sub-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (output 0)
          (- (input 0) (input 1)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-equal-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :successors 2)
    (if (= (input 0)
           (input 1))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-less-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :successors 2)
    (if (< (input 0)
           (input 1))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-not-greater-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :successors 2)
    (if (<= (input 0)
            (input 1))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-divide-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 2)
    (multiple-value-bind (quotient remainder)
        (floor (input 0) (input 1))
      (setf (output 0) quotient)
      (setf (output 1) remainder)
      (successor 0))))
