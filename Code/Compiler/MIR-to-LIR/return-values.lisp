(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:initialize-values-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :output *rdi*))

(defmethod process-instruction
    ((instruction cleavir-ir:set-return-value-instruction)
     lexical-locations)
  (multiple-value-bind (index-input value-location)
      (cleavir-ir:inputs instruction)
    ;; We can only handle constant inputs for now.
    (assert (typep index-input 'cleavir-ir:constant-input))
    (let ((index (cleavir-ir:value index-input)))
      (if (< index 5)
          (let ((register
                  (case index (0 *rax*) (1 *rdx*) (2 *rcx*) (3 *rsi*) (4 *r9*))))
            (change-class instruction 'cleavir-ir:assignment-instruction
                          :input value-location
                          :output register))
          (let ((offset-input (make-instance 'cleavir-ir:immediate-input
                                :value (* 8 (- index 5)))))
            (cleavir-ir:insert-instruction-before
             (make-instance 'cleavir-ir:assignment-instruction
               :input *rsp*
               :output *r11*)
             instruction)
            (cleavir-ir:insert-instruction-before
             (make-instance 'cleavir-ir:unsigned-sub-instruction
               :inputs (list *r11* offset-input)
               :output *r11*)
             instruction)
            (change-class instruction 'cleavir-ir:memset1-instruction
                          :address *r11*
                          :value value-location))))))

(defmethod process-instruction
    ((instruction cleavir-ir:compute-return-value-count-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :input *rdi*))

