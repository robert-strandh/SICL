(cl:in-package #:sicl-argument-processing)

(defun call-error (error-name
                   error-function-location
                   dynamic-environment-location
                   &rest arguments)
  (let ((unreachable
          (make-instance 'cleavir-ir:unreachable-instruction
            :dynamic-environment-location dynamic-environment-location))
        (error-name-input
          (make-instance 'cleavir-ir:constant-input :value error-name)))
    (make-instance 'cleavir-ir:funcall-instruction
      :dynamic-environment-location dynamic-environment-location
      :inputs (list* error-function-location error-name-input arguments)
      :successor unreachable)))
