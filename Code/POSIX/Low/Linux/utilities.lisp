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

(defun adjust-return-value ()
  (let ((label-normal (make-instance 'cluster:label))
        (label-out (make-instance 'cluster:label)))
    (list
     ;; Check whether returned value is non-negative.
     (make-instance 'cluster:code-command
       :mnemonic "CMP"
       :operands
       (list
        (make-instance 'cluster:gpr-operand
          :code-number *rax*
          :size 64)
        (make-instance 'cluster:immediate-operand
          :value 0)))
     ;; If it is, then jump.
     (make-instance 'cluster:code-command
       :mnemonic "JGE"
       :operands (list label-normal))
     ;; Come here if the return value is negative,
     ;; indicating an error.  Then the error number
     ;; is the negation of the value in RAX.  Move it
     ;; to RDX so that it becomes the second return value.
     (make-instance 'cluster:code-command
       :mnemonic "MOV"
       :operands
       (list
        (make-instance 'cluster:gpr-operand
          :code-number *rdx*
          :size 64)
       (make-instance 'cluster:gpr-operand
          :code-number *rax*
          :size 64)))
     ;; Then set the first return value to -1.
     (make-instance 'cluster:code-command
       :mnemonic "MOV"
       :operands
       (list
        (make-instance 'cluster:gpr-operand
          :code-number *rax*
          :size 64)
        (make-instance 'cluster:immediate-operand
          :value -1)))
     ;; And skip the normal return-value processing
     (make-instance 'cluster:code-command
       :mnemonic "JMP"
       :operands (list label-out))
     ;; Come here if the return value is non negative.
     label-normal
     ;; Set the second return value to 0 for aesthetic
     ;; reasons.
     (make-instance 'cluster:code-command
       :mnemonic "MOV"
       :operands
       (list
        (make-instance 'cluster:gpr-operand
          :code-number *rdx*
          :size 64)
        (make-instance 'cluster:immediate-operand
          :value 0)))
     label-out)))

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

(defun rack-from-standard-object (destination-register source-register)
  (list
   (make-instance 'cluster:code-command
     :mnemonic "MOV"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number destination-register
        :size 64)
      (make-instance 'cluster:memory-operand
        :base-register source-register
        :displacement 3
        :size 64)))))

(defun first-vector-element-from-rack (register)
  (list
   (make-instance 'cluster:code-command
     :mnemonic "ADD"
     :operands
     (list
      (make-instance 'cluster:gpr-operand
        :code-number register
        :size 64)
      (make-instance 'cluster:immediate-operand
        ;; We have 5 words before the element starts,
        ;; and we subtract the tag of the rack.
        :value (- (* 5 8) 7))))))
