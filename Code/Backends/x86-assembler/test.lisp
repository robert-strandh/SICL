(in-package :x86-assembler)

(defun run-test-1 ()
  (let* ((x (list (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 1
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))
		  (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 2
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))
		  (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 3
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))
		  (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 4
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))
		  (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 5
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))
		  (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 6
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))
		  (make-instance 'code-command
		    :mnemonic "ADD"
		    :operands (list
			       (make-instance 'gpr-operand
				 :code-number 7
				 :size 32)
			       (make-instance 'immediate-operand
				 :value 33)))))
	 (result '((#x83 #xC1 33)
		   (#x83 #xC2 33)
		   (#x83 #xC3 33)
		   (#x83 #xC4 33)
		   (#x83 #xC5 33)
		   (#x83 #xC6 33)
		   (#x83 #xC7 33))))
    (assert (equal result (assemble x)))))

(defun run-tests ()
  (run-test-1))
