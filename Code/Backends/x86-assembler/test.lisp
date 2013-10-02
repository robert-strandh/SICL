(in-package :x86-assembler)

(defun run-test-83 (mnemonic extension reg)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 32)
			(make-instance 'immediate-operand
			  :value 33))))
	(y `(#x83 ,(+ #xC0 (ash extension 3) reg) 33)))
    (assert (equal (list y) (assemble (list x))))))

(defun run-test-add-83

(defun run-test-1 ()
  (loop for mnemonic in '("ADD" "OR" "AND" "XOR")
	for extension in '(0 1 4 6)
	do (loop for reg from 1 to 7
		 do (run-test-83 mnemonic extension reg))))

(defun run-tests ()
  (run-test-1))
