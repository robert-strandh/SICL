(cl:in-package #:sicl-argument-processing)

(defun check-minimum-argument-count (argument-count-location
                                     minimum-argument-count
                                     dynamic-environment-location
                                     error-function-location)
  (let* ((minimum-argument-count-input (make-instance 'cleavir-ir:constant-input
                                         :value minimum-argument-count))
         (error (call-error 'too-few-arguments
                            error-function-location
                            dynamic-environment-location
                            argument-count-location
                            minimum-argument-count-input))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location)))
    (values (make-instance 'cleavir-ir:fixnum-less-instruction
              :dynamic-environment-location dynamic-environment-location
              :inputs (list argument-count-location minimum-argument-count-input)
              :successors (list error nop))
            nop)))
