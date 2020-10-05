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
     (instruction cleavir-ir:fixnum-multiply-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 2)
    (multiple-value-bind (most-significant least-significant)
        (floor (* (input 0) (input 1))
               ;; FIXME: make this number part of some configuration.
               #.(expt 2 62))
      (setf (output 0) most-significant)
      (setf (output 1) least-significant)
      (successor 0))))

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

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-logand-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (output 0)
          (logand (input 0) (input 1)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-logior-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (output 0)
          (logior (input 0) (input 1)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-logxor-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :outputs 1)
    (setf (output 0)
          (logxor (input 0) (input 1)))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnum-lognot-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0)
          (lognot (input 0)))
    (successor 0)))
