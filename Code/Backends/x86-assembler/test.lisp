(in-package :x86-assembler)

(defun run-test-1 ()
  (let* ((x (loop for reg from 1 to 7
		  collect (make-instance 'code-command
			    :mnemonic "ADD"
			    :operands (list
				       (make-instance 'gpr-operand
					 :code-number reg
					 :size 32)
				       (make-instance 'immediate-operand
					 :value 33)))))
	 (y (loop for reg from 1 to 7
		  collect `(#x83 ,(+ #xC0 reg) 33))))
    (assert (equal y (assemble x)))))

(defun run-tests ()
  (run-test-1))
