(cl:in-package #:sicl-posix-low)

(defparameter *exit-commands*
  (list
   ;; We are given the exit status as a fixnum in the first argument,
   ;; so we start by shifting it one position to get a native integer.
   (make-instance 'cluster:code-command
     :mnemonic "SHR"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number *rdi*
        :size 64)
      (make-instance 'cluster:immediate-operand
        :value 1)))
   ;; System call numer
   (make-instance 'cluster:code-command
     :mnemonic "MOV"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number *rax*
        :size 64)
      (make-instance 'cluster:immediate-operand
        :value 60)))
   (make-instance 'cluster:code-command
     :mnemonic "SYSCALL"
     :operands '())))
