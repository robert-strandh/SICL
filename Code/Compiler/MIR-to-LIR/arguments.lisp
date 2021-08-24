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
    ;; locations, then an argument count if spilling all arguments to
    ;; the stack, arguments, the call site descriptor, the return
    ;; address, and finally the caller RBP.
    ;;
    ;; So, in order to get at the N'th argument (when N is boxed), we
    ;; load [RSP + 4N + Offset] where Offset is an offset which will
    ;; be described next.  If N is constant, then we compute (4N +
    ;; Offset) and use the result as a displacement.  Else, we use the
    ;; forementioned effective address.
    ;;
    ;; The final thing to note is how to compute the Offset.
    ;; Typically, we do not have to spill argument registers to the
    ;; stack, and only the sixth and later arguments are on the stack;
    ;; so Offset is defined as 8 * Slots - 40.  If we do spill
    ;; argument registers (typically for parsing arguments with a
    ;; lambda list containing &key or &rest), the argument count is
    ;; placed on the stack, and so Offset must account for a slot for
    ;; the argument count, and that the arguments start with the first
    ;; argument.  So, in the latter case, Offset is instead defined to
    ;; be 8 * Slots + 8.
    (let ((offset (if *spill-arguments-p*
                      (* 8 (1+ *stack-slots*))
                      (- (* 8 *stack-slots*)
                         (* 8 (length x86-64:*argument-registers*))))))
      (change-class instruction
        'sicl-ir:memref-effective-address-instruction
        :inputs (etypecase input
                  (cleavir-ir:immediate-input
                   (list x86-64:*rsp*
                         (cleavir-ir:make-immediate-input 1)
                         (sicl-ir:nowhere)
                         (cleavir-ir:make-immediate-input
                          (+ (* 4 (cleavir-ir:value input))
                             offset))))
                  (cleavir-ir:register-location
                   (list x86-64:*rsp*
                         (cleavir-ir:make-immediate-input 4)
                         input
                         (cleavir-ir:make-immediate-input
                          offset))))))))
