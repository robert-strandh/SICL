(cl:in-package #:sicl-argument-processing)

(defun add-one-optional-layer (no-more-branch
                               more-branch
                               parameter-location
                               supplied-p-location
                               argument-count-location
                               index
                               dynamic-environment-location)
  (let* ((assign-nil-to-supplied-p
           (make-instance 'cleavir-ir:assignment-instruction
             :input (make-instance 'cleavir-ir:constant-input :value nil)
             :output supplied-p-location
             :successor no-more-branch
             :dynamic-environment-location dynamic-environment-location))
         (assign-nil-to-parameter
           (make-instance 'cleavir-ir:assignment-instruction
             :input (make-instance 'cleavir-ir:constant-input :value nil)
             :output parameter-location
             :successor assign-nil-to-supplied-p
             :dynamic-environment-location dynamic-environment-location))
         (assign-t-to-supplied-p
           (make-instance 'cleavir-ir:assignment-instruction
             :input (make-instance 'cleavir-ir:constant-input :value t)
             :output supplied-p-location
             :successor more-branch
             :dynamic-environment-location dynamic-environment-location))
         (initialize-parameter
           (make-instance 'cleavir-ir:argument-instruction
             :input (make-instance 'cleavir-ir:constant-input :value index)
             :output parameter-location
             :successor assign-t-to-supplied-p
             :dynamic-environment-location dynamic-environment-location))
         (test
           (make-instance 'cleavir-ir:fixnum-not-greater-instruction
             :inputs (list argument-count-location
                           (make-instance 'cleavir-ir:constant-input :value index))
             :successors (list assign-nil-to-parameter initialize-parameter)
             :dynamic-environment-location dynamic-environment-location)))
    (values assign-nil-to-parameter test)))
             
(defun initialize-optional-parameters
    (lexical-locations argument-count-location dynamic-environment-location end-index)
  (let* ((nop-no-more (make-instance 'cleavir-ir:nop-instruction
                        :dynamic-environment-location dynamic-environment-location))
         (no-more-branch nop-no-more)
         (nop-more (make-instance 'cleavir-ir:nop-instruction
                     :dynamic-environment-location dynamic-environment-location))
         (more-branch
           (make-instance 'cleavir-ir:fixnum-not-greater-instruction
             :inputs (list argument-count-location
                           (make-instance 'cleavir-ir:constant-input :value end-index))
             :successors (list nop-no-more nop-more)
             :dynamic-environment-location dynamic-environment-location)))
    (loop for index downfrom (1- end-index)
          for (optional supplied-p) in (reverse lexical-locations)
          do (multiple-value-bind (no-more more)
                 (add-one-optional-layer no-more-branch
                                         more-branch
                                         optional
                                         supplied-p
                                         argument-count-location
                                         index
                                         dynamic-environment-location)
               (setf no-more-branch no-more)
               (setf more-branch more)))
    (values more-branch nop-no-more nop-more)))
