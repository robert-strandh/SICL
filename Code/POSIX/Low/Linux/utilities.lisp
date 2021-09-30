(cl:in-package #:sicl-posix-low)

(defun syscall (syscall-number)
  (list
   (make-instance 'cluster:code-command
     :mnemonic "MOV"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number *rax*
        :size 64)
      (make-instance 'cluster:immediate-operand
        :value syscall-number)))
   (make-instance 'cluster:code-command
     :mnemonic "SYSCALL"
     :operands '())))

(defun return-to-caller ()
  (list
   ;; Set number of return values.
   (make-instance 'cluster:code-command
     :mnemonic "MOV"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number *rdi*
        :size 64)
      (make-instance 'cluster:immediate-operand
        :value 2)))
   ;; Return to caller.
   (make-instance 'cluster:code-command
     :mnemonic "RET"
     :operands '())))

(defun unbox-fixnum (register)
  (list
   (make-instance 'cluster:code-command
     :mnemonic "SHR"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number register
        :size 64)
      (make-instance 'cluster:immediate-operand
        :value 1)))))
