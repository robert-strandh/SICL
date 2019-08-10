(cl:in-package #:sicl-argument-processing)

(defun initialize-keyword-parameters-to-nil
    (lexical-locations dynamic-environment-location)
  (loop with nop = (make-instance 'cleavir-ir:nop-instruction
                     :dynamic-environment-location dynamic-environment-location)
        with first = nop
        for (key supplied-p) in (reverse lexical-locations)
        for nil-location = (make-instance 'cleavir-ir:constant-input :value nil)
        do (setf first
                 (make-instance 'cleavir-ir:assignment-instruction
                   :input nil-location
                   :output supplied-p
                   :successor (make-instance 'cleavir-ir:assignment-instruction
                                :input nil-location
                                :output key
                                :successor first
                                :dynamic-environment-location dynamic-environment-location)
                   :dynamic-environment-location dynamic-environment-location))
        finally (return (values first nop))))
