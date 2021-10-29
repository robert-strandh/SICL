(cl:in-package #:sicl-argument-processing)

(defun check-maximum-argument-count
    (argument-count-location
     maximum-argument-count
     origin
     dynamic-environment-location)
  (let* ((maximum-argument-count-input (make-instance 'cleavir-ir:constant-input
                                         :value maximum-argument-count))
         (error (call-error 'too-many-arguments
                            dynamic-environment-location
                            origin
                            (make-instance 'cleavir-ir:constant-input :value :argument-count)
                            argument-count-location
                            (make-instance 'cleavir-ir:constant-input :value :maximum-argument-count)
                            maximum-argument-count-input))
         (nop (make-instance 'cleavir-ir:nop-instruction
                :dynamic-environment-location dynamic-environment-location)))
    (values (make-instance 'cleavir-ir:fixnum-not-greater-instruction
              :dynamic-environment-location dynamic-environment-location
              :inputs (list argument-count-location maximum-argument-count-input)
              :successors (list nop error))
            nop)))
