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
    ;; stack, and to adjust the frame pointer.  At this point, the
    ;; stack contains (going from RSP to RBP) space for spilled
    ;; locations, then arguments, the call site descriptor, the return
    ;; address, and finally the caller RBP.
    ;;
    ;; So, in order to get at the N'th argument (when N is boxed), we
    ;; load [RSP + 4N + 8Slots] where Slots is the number of
    ;; stack slots used.  If N is constant, then we compute (4N +
    ;; Slots) and use the result as a displacement.  Else, we use
    ;; the forementioned effective address.
    (change-class instruction
      'sicl-ir:memref-effective-address-instruction
      :inputs (etypecase input
                (cleavir-ir:immediate-input
                 (list x86-64:*rsp*
                       (cleavir-ir:make-immediate-input 1)
                       (sicl-ir:nowhere)
                       (cleavir-ir:make-immediate-input
                        (+ (* 4 (cleavir-ir:value input))
                           (* 8 *stack-slots*)))))
                (cleavir-ir:register-location
                 (list x86-64:*rsp*
                       (cleavir-ir:make-immediate-input 4)
                       input
                       (cleavir-ir:make-immediate-input
                        (* 8 *stack-slots*))))))))
