(in-package :x86-assembler)

(defun run-test-83 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 32)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x83
	     ,(+ #xC0 (ash extension 3) reg)
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))

(defun run-test-1 ()
  (loop for mnemonic in '("ADD" "OR" "AND" "XOR")
	for extension in '(0 1 4 6)
	do (loop for reg from 1 to 7
		 do (loop for imm from -128 to 127
			  do (run-test-83 mnemonic extension reg imm)))))

(defun run-tests ()
  (run-test-1))
