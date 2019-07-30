(cl:in-package #:sicl-argument-processing)

(defun check-even-number-of-keyword-arguments (argument-count-location
                                               dynamic-environment-location
                                               error-function-location
                                               first-index)
  (let* ((remaining-argument-count-location
           (make-instance 'cleavir-ir:lexical-location :name 'remainin-argument-count))
         (quotient-location
           (make-instance 'cleavir-ir:lexical-location :name 'quotient))
         (remainder-location
           (make-instance 'cleavir-ir:lexical-location :name 'remainder))
         (constant-input-2
           (make-instance 'cleavir-ir:constant-input :value 2))
         (constant-input-0
           (make-instance 'cleavir-ir:constant-input :value 0))
         (first-index-location (make-instance 'cleavir-ir:constant-input :value first-index))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location))
         (error-branch
           (call-error 'odd-number-of-keyword-arguments
                       error-function-location
                       dynamic-environment-location
                       remaining-argument-count-location)))
    (let ((first (make-instance 'cleavir-ir:fixnum-equal-instruction
                   :inputs (list constant-input-0 remainder-location)
                   :successors (list nop error-branch)
                   :dynamic-environment-location dynamic-environment-location)))
      (setf first
            (make-instance 'cleavir-ir:fixnum-divide-instruction
              :rounding-mode 'floor
              :inputs (list remaining-argument-count-location constant-input-2)
              :outputs (list quotient-location remainder-location)
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:fixnum-sub-instruction
              :inputs (list argument-count-location first-index-location)
              :outputs (list quotient-location remainder-location)
              :successors (list first first)
              :dynamic-environment-location dynamic-environment-location))
      (values first nop))))
