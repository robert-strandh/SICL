(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:initialize-values-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :output *rdi*))

;;; We can only handle immediate inputs for now.  Recall that the
;;; immediate input was converted from a constant input, and the
;;; constant input was a non-negative fixnum indicating the index.
;;; That fixnum was converted to an immediate input by HIR-to-MIR, so
;;; its value is now twice what it used to be.  For that reason, we
;;; essentially need to convert it back before processing it.

(defmethod process-instruction
    ((instruction cleavir-ir:set-return-value-instruction)
     lexical-locations)
  (destructuring-bind (index-input value-location)
      (cleavir-ir:inputs instruction)
    (assert (typep index-input 'cleavir-ir:immediate-input))
    (let ((index (ash (cleavir-ir:value index-input) -1)))
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

(defmethod process-instruction
    ((instruction cleavir-ir:return-value-instruction)
     lexical-locations)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (assert (typep input 'cleavir-ir:immediate-input))
    (let ((index (ash (cleavir-ir:value input) -1)))
      (if (< index 5)
          (let ((register
                  (case index (0 *rax*) (1 *rdx*) (2 *rcx*) (3 *rsi*) (4 *r9*))))
            (change-class instruction 'cleavir-ir:assignment-instruction
                          :input register))
          (let ((offset-input (make-instance 'cleavir-ir:immediate-input
                                :value (* 8 (- index 4)))))
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
            (change-class instruction 'cleavir-ir:memref1-instruction
                          :address *r11*))))))
