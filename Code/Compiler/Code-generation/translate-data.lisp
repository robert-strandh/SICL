(cl:in-package #:sicl-code-generation)

(defgeneric translate-datum (datum))

(defmethod translate-datum ((datum cleavir-ir:immediate-input))
  (make-instance 'cluster:immediate-operand
    :value (cleavir-ir:value datum)))

(defmethod translate-datum ((datum cleavir-ir:register-location))
  (make-instance 'cluster:gpr-operand
    :code-number (position datum
                  (list sicl-mir-to-lir:*rax*
                        sicl-mir-to-lir:*rbx*
                        sicl-mir-to-lir:*rcx*
                        sicl-mir-to-lir:*rdx*
                        sicl-mir-to-lir:*rsp*
                        sicl-mir-to-lir:*rbp*
                        sicl-mir-to-lir:*rsi*
                        sicl-mir-to-lir:*rdi*
                        sicl-mir-to-lir:*r8*
                        sicl-mir-to-lir:*r9*
                        sicl-mir-to-lir:*r10*
                        sicl-mir-to-lir:*r11*
                        sicl-mir-to-lir:*r12*
                        sicl-mir-to-lir:*r13*
                        sicl-mir-to-lir:*r14*
                        sicl-mir-to-lir:*r15*))
    :size 64))
