(cl:in-package #:sicl-code-generation)

(defgeneric translate-datum (datum))

(defmethod translate-datum ((datum cleavir-ir:immediate-input))
  (make-instance 'cluster:immediate-operand
    :value (cleavir-ir:value datum)))

(defun register-code-number (register-location)
  (position register-location
            (list x86-64:*rax*
                  x86-64:*rbx*
                  x86-64:*rcx*
                  x86-64:*rdx*
                  x86-64:*rsp*
                  x86-64:*rbp*
                  x86-64:*rsi*
                  x86-64:*rdi*
                  x86-64:*r8*
                  x86-64:*r9*
                  x86-64:*r10*
                  x86-64:*r11*
                  x86-64:*r12*
                  x86-64:*r13*
                  x86-64:*r14*
                  x86-64:*r15*)))

(defmethod translate-datum ((datum cleavir-ir:register-location))
  (make-instance 'cluster:gpr-operand
    :code-number (register-code-number datum)
    :size 64))

(defun translate-base-register (register-location)
  (register-code-number register-location))
