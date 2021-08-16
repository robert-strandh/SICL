(cl:in-package #:sicl-mir-to-lir)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:argument-instruction))
  ;; We have to replace the instruction with either an assignment from
  ;; the right register or stack location (if the input is an
  ;; immediate value), or an instruction sequence which picks the
  ;; right location at runtime.
  (destructuring-bind (input)
      (cleavir-ir:inputs instruction)
    ;; As per section 27.7 of the SICL specification, we will arrange
    ;; for a prologue to copy the arguments stored in registers to the
    ;; stack, and to adjust the frame pointer.  Really, this sequence
    ;; should be MOV Result, [RSP - Index * 4].  Note that the index
    ;; is a boxed fixnum, so we divide the scale by 2 to "unbox" it.
    (destructuring-bind (result)
        (cleavir-ir:outputs instruction)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:assignment-instruction
         :inputs (list result)
         :outputs (list input))
       instruction)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:negate-instruction
         :inputs (list result)
         :outputs (list result))
       instruction)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:shift-left-instruction
         :shift-count (cleavir-ir:make-immediate-input 2)
         :shifted-input result
         :outputs (list result))
       instruction)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:fixnum-add-instruction
         :inputs (list result x86-64:*rsp*)
         :outputs (list result))
       instruction)
      (change-class instruction
                    'cleavir-ir:memref1-instruction
                    :inputs (list result)))))
