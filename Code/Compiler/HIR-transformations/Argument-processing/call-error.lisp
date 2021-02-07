(cl:in-package #:sicl-argument-processing)

(defun call-error (error-name
                   dynamic-environment-location
                   &rest arguments)
  (let ((unreachable
          (make-instance 'cleavir-ir:unreachable-instruction
            :dynamic-environment-location dynamic-environment-location))
        (error-name-input
          (make-instance 'cleavir-ir:constant-input :value error-name)))
    (make-instance 'cleavir-ir:named-call-instruction
      :dynamic-environment-location dynamic-environment-location
      :callee-name 'error
      :inputs (cons error-name-input arguments)
      :successor unreachable)))
