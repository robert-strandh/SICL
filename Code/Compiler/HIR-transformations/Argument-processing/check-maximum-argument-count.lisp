(cl:in-package #:sicl-argument-processing)

(defun check-maximum-argument-count (argument-count-location
                                     maximum-argument-count
                                     dynamic-environment-location
                                     error-function-location)
  (let* ((maximum-argument-count-input (make-instance 'cleavir-ir:constant-input
                                         :value maximum-argument-count))
         (error (call-error 'too-manu-arguments
                            error-function-location
                            dynamic-environment-location
                            argument-count-location
                            maximum-argument-count-input))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location)))
    (values (make-instance 'cleavir-ir:fixnum-not-greater-instruction
              :dynamic-environment-location dynamic-environment-location
              :inputs (list argument-count-location maximum-argument-count-input)
              :successors (list nop error))
            nop)))
