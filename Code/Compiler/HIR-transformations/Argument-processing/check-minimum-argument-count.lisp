(cl:in-package #:sicl-argument-processing)

(defun check-minimum-argument-count (argument-count-location
                                     required-argument-count
                                     dynamic-environment-location
                                     error-function-location)
  (let* ((required-argument-count-input (make-instance 'cleavir-ir:constant-input
                                          :value required-argument-count))
         (error (call-error 'too-few-arguments
                            error-function-location
                            dynamic-environment-location
                            argument-count-location
                            required-argument-count-input)))
    (make-instance 'cleavir-ir:fixnum-less-instruction
      :dynamic-environment-location dynamic-environment-location
      :inputs (list argument-count-location required-argument-count-input)
      :successors (list error error))))
