(cl:in-package #:sicl-code-generation)

(defgeneric translate-datum (datum))

(defmethod translate-datum ((datum cleavir-ir:immediate-input))
  (make-instance 'cluster:immediate-operand
    :value (cleavir-ir:value datum)))

(defmethod translate-datum ((datum cleavir-ir:register-location))
  (make-instance 'cluster:gpr-operand
    :code-number (case datum
                   (sicl-mir-to-lir:*rax* 0)
                   (sicl-mir-to-lir:*rbx* 1)
                   (sicl-mir-to-lir:*rcx* 2)
                   (sicl-mir-to-lir:*rdx* 3)
                   (sicl-mir-to-lir:*rsp* 4)
                   (sicl-mir-to-lir:*rbp* 5)
                   (sicl-mir-to-lir:*rsi* 6)
                   (sicl-mir-to-lir:*rdi* 7)
                   (sicl-mir-to-lir:*r8* 8)
                   (sicl-mir-to-lir:*r9* 9)
                   (sicl-mir-to-lir:*r10* 10)
                   (sicl-mir-to-lir:*r11* 11)
                   (sicl-mir-to-lir:*r12* 12)
                   (sicl-mir-to-lir:*r13* 13)
                   (sicl-mir-to-lir:*r14* 14)
                   (sicl-mir-to-lir:*r15* 15))
    :size 64))
