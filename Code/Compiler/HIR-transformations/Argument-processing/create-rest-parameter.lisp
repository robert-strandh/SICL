(cl:in-package #:sicl-argument-processing)

(defun create-rest-parameter (argument-count-location
                              rest-parameter-location
                              dynamic-environment-location
                              first-index)
  (let ((argument-location (make-instance 'cleavir-ir:lexical-location :name (gensym "argument")))
        (index-location (make-instance 'cleavir-ir:lexical-location :name (gensym "index")))
        (constant-1-input (make-instance 'cleavir-ir:constant-input :value 1))
        (first-index-input (make-instance 'cleavir-ir:constant-input :value first-index))
        (nop (make-instance 'cleavir-ir:nop-instruction
               :dynamic-environment-location dynamic-environment-location)))
    (let* ((first (make-instance 'cleavir-ir:fixnum-sub-instruction
                    :inputs (list index-location constant-1-input)
                    :output index-location
                    :dynamic-environment-location dynamic-environment-location))
           (temp first))
      (setf first
            (make-instance 'cleavir-ir:multiple-to-fixed-instruction
              :output rest-parameter-location
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:named-call-instruction
              :callee-name 'cons
              :inputs (list argument-location rest-parameter-location)
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:argument-instruction
              :input index-location
              :output argument-location
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:fixnum-less-instruction
              :inputs (list index-location first-index-input)
              :successors (list nop first)
              :dynamic-environment-location dynamic-environment-location))
      (setf (cleavir-ir:successors temp)
            (list first first))
      (setf first
            (make-instance 'cleavir-ir:fixnum-sub-instruction
              :inputs (list argument-count-location constant-1-input)
              :output index-location
              :successors (list first first)
              :dynamic-environment-location dynamic-environment-location))
      (values first nop))))
