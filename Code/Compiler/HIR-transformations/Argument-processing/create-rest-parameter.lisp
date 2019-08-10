(cl:in-package #:sicl-argument-processing)

(defun create-rest-parameter (argument-count-location
                              rest-parameter-location
                              dynamic-environment-location
                              first-index)
  (let ((cons-location (make-instance 'cleavir-ir:lexical-location :name (gensym "cons")))
        (argument-location (make-instance 'cleavir-ir:lexical-location :name (gensym "argument")))
        (index-location (make-instance 'cleavir-ir:lexical-location :name (gensym "index")))
        (constant-1-input (make-instance 'cleavir-ir:constant-input :value 1))
        (constant-cons-input (make-instance 'cleavir-ir:constant-input :value 'cons))
        (values-location (make-instance 'cleavir-ir:values-location))
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
              :input values-location
              :output rest-parameter-location
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (setf first
            (make-instance 'cleavir-ir:funcall-instruction
              :inputs
              (list cons-location argument-location rest-parameter-location)
              :output values-location
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
      (setf first
            (make-instance 'cleavir-ir:fdefinition-instruction
              :input constant-cons-input
              :output cons-location
              :successor first
              :dynamic-environment-location dynamic-environment-location))
      (values first nop))))
