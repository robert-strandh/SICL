(cl:in-package #:sicl-argument-processing)

(defun initialize-required-parameters
    (lexical-locations dynamic-environment-location)
  (let ((count (length lexical-locations))
        (locations (reverse lexical-locations)))
    (assert (plusp count))
    (flet ((make-one-instruction (index location successor)
             (let ((input (make-instance 'cleavir-ir:constant-input :value index)))
               (make-instance 'cleavir-ir:argument-instruction
                 :dynamic-environment-location dynamic-environment-location
                 :input input
                 :output location
                 :successor successor))))
      (let* ((last (make-one-instruction (1- count) (first locations) nil))
             (first (loop with first = last
                          for i downfrom (- count 2)
                          for location in (rest locations)
                          do (setf first
                                   (make-one-instruction i location first))
                          finally (return first))))
        (values first last)))))
                 

