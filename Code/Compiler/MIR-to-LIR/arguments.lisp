(cl:in-package #:sicl-mir-to-lir)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:argument-instruction))
  ;; We have to replace the instruction with either an assignment from
  ;; the right register or stack location (if the input is an
  ;; immediate value), or an instruction sequence which picks the
  ;; right location at runtime.
  (destructuring-bind (input)
      (cleavir-ir:inputs instruction)
    (cond
      ((typep input 'cleavir-ir:immediate-input)
       (let ((value     (cleavir-ir:value input))
             (registers (length x86-64:*argument-registers*)))
         (if (< value registers)
             (change-class instruction
                           'cleavir-ir:assignment-instruction
                           :inputs (list (nth value x86-64:*argument-registers*)))
             (change-class instruction
                           'cleavir-ir:memref2-instruction
                           :inputs (list x86-64:*rsp*
                                         (cleavir-ir:make-immediate-input
                                          (* 8 (- value (1- registers)))))))))
      (t
       ;; As per section 27.7 of the SICL specification, we will
       ;; arrange for a prologue to copy the arguments stored in
       ;; registers to the stack, and to adjust the frame pointer.
       ;; Really, this sequence should be MOV Result, [RSP - Index * 8]
       (destructuring-bind (result)
           (cleavir-ir:outputs instruction)
         (cleavir-ir:insert-instruction-before
          (make-instance 'cleavir-ir:assignment-instruction
            :inputs (list result)
            :outputs (list input))
          instruction)
         (cleavir-ir:insert-instruction-before
          (make-instance 'cleavir-ir:fixnum-multiply-instruction
            :inputs (list result (cleavir-ir:make-immediate-input -8))
            :outputs (list result))
          instruction)
         (cleavir-ir:insert-instruction-before
          (make-instance 'cleavir-ir:fixnum-add-instruction
            :inputs (list result x86-64:*rsp*)
            :outputs (list result))
          instruction)
         (change-class instruction
                       'cleavir-ir:memref1-instruction
                       :inputs (list result)))))))
