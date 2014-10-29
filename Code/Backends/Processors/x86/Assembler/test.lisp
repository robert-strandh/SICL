(in-package :x86-assembler)

;;; Test instructions with an opcode of #x83 and where the
;;; first operand is a 32-bit GPR between 1 and 7.
(defun run-test-83-reg-1 (mnemonic extension reg imm)
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

;;; Test instructions with an opcode of #x83 and where the
;;; first operand is a 32-bit GPR between 8 and 15.
(defun run-test-83-reg-2 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 32)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x41 ; Rex prefix
	     #x83
	     ,(+ #xC0 (ash extension 3) (- reg 8))
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))

;;; Test instructions with an opcode of #x83 and where the
;;; first operand is a 16-bit GPR between 1 and 7.
(defun run-test-83-reg-3 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 16)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x66 ; Operand-size override prefix.
	     #x83
	     ,(+ #xC0 (ash extension 3) reg)
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))

;;; Test instructions with an opcode of #x83 and where the
;;; first operand is a 16-bit GPR between 8 and 15.
(defun run-test-83-reg-4 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 16)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x66 ; Operand-size override prefix.
	     #x41 ; Rex prefix
	     #x83
	     ,(+ #xC0 (ash extension 3) (- reg 8))
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))

;;; Test instructions with an opcode of #x83 and where the
;;; first operand is a 64-bit GPR between 1 and 7.
(defun run-test-83-reg-5 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 64)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x48 ; Rex prefix
	     #x83
	     ,(+ #xC0 (ash extension 3) reg)
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))

;;; Test instructions with an opcode of #x83 and where the
;;; first operand is a 64-bit GPR between 8 and 15.
(defun run-test-83-reg-6 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'gpr-operand
			  :code-number reg
			  :size 64)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x49 ; Rex prefix
	     #x83
	     ,(+ #xC0 (ash extension 3) (- reg 8))
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))


(defun run-test-83-reg ()
  (loop for mnemonic in '("ADD" "OR" "AND" "SUB" "XOR")
	for extension in '(0 1 4 5 6)
	do (loop for reg from 1 to 7
		 do (loop for imm from -128 to 127
			  do (run-test-83-reg-1 mnemonic extension reg imm)
			     (run-test-83-reg-3 mnemonic extension reg imm)
			     (run-test-83-reg-5 mnemonic extension reg imm)))
	   (loop for reg from 8 to 15
		 do (loop for imm from -128 to 127
			  do (run-test-83-reg-2 mnemonic extension reg imm)
			     (run-test-83-reg-4 mnemonic extension reg imm)
			     (run-test-83-reg-6 mnemonic extension reg imm)))))

;;; Test instructions with an opcode of #x83 and where the first
;;; operand is a 32-bit memory operand with only a base register, one
;;; of 0, 1, 2, 3, 6, and 7.
(defun run-test-83-mem-1 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'memory-operand
			  :base-register reg
			  :size 32)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x83
	     ,(+ #x00 (ash extension 3) reg)
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))
  
;;; Test instructions with an opcode of #x83 and where the first
;;; operand is a 32-bit memory operand with only a base register, one
;;; of 8, 9, 10, 11, 14, and 15.
(defun run-test-83-mem-2 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'memory-operand
			  :base-register reg
			  :size 32)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x41 ; Rex prefix.
	     #x83
	     ,(+ #x00 (ash extension 3) (- reg 8))
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))
  
;;; Test instructions with an opcode of #x83 and where the first
;;; operand is a 16-bit memory operand with only a base register, one
;;; of 0, 1, 2, 3, 6, and 7.
(defun run-test-83-mem-3 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'memory-operand
			  :base-register reg
			  :size 16)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x66 ; Operand-size override prefix.
	     #x83
	     ,(+ #x00 (ash extension 3) reg)
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))
  
;;; Test instructions with an opcode of #x83 and where the first
;;; operand is a 16-bit memory operand with only a base register, one
;;; of 8, 9, 10, 11, 14, and 15.
(defun run-test-83-mem-4 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'memory-operand
			  :base-register reg
			  :size 16)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x66 ; Operand-size override prefix.
	     #x41 ; Rex prefix.
	     #x83
	     ,(+ #x00 (ash extension 3) (- reg 8))
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))
  
;;; Test instructions with an opcode of #x83 and where the first
;;; operand is a 64-bit memory operand with only a base register, one
;;; of 0, 1, 2, 3, 6, and 7.
(defun run-test-83-mem-5 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'memory-operand
			  :base-register reg
			  :size 64)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x48 ; Rex prefix
	     #x83
	     ,(+ #x00 (ash extension 3) reg)
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))
  
;;; Test instructions with an opcode of #x83 and where the first
;;; operand is a 64-bit memory operand with only a base register, one
;;; of 8, 9, 10, 11, 14, and 15.
(defun run-test-83-mem-6 (mnemonic extension reg imm)
  (let ((x (make-instance 'code-command
	     :mnemonic mnemonic
	     :operands (list
			(make-instance 'memory-operand
			  :base-register reg
			  :size 64)
			(make-instance 'immediate-operand
			  :value imm))))
	(y `(#x49 ; Rex prefix.
	     #x83
	     ,(+ #x00 (ash extension 3) (- reg 8))
	     ,(if (minusp imm) (+ imm 256) imm))))
    (assert (equal (list y) (assemble (list x))))))

(defun run-test-83-mem ()
  (loop for mnemonic in '("ADD" "OR" "AND" "SUB" "XOR")
	for extension in '(0 1 4 5 6)
	do (loop for reg in '(0 1 2 3 6 7)
		 do (loop for imm from -128 to 127
			  do (run-test-83-mem-1 mnemonic extension reg imm)
			     (run-test-83-mem-3 mnemonic extension reg imm)
			     (run-test-83-mem-5 mnemonic extension reg imm)))
	   (loop for reg in '(8 9 10 11 14 15)
		 do (loop for imm from -128 to 127
			  do (run-test-83-mem-2 mnemonic extension reg imm)
			     (run-test-83-mem-4 mnemonic extension reg imm)
			     (run-test-83-mem-6 mnemonic extension reg imm)))))

(defun run-test-83 ()
  (run-test-83-reg)
  (run-test-83-mem))

(defun run-tests ()
  (run-test-83))

(defparameter *t*
  (let ((label1 (make-instance 'label))
	(label2 (make-instance 'label)))
    (list
     (make-instance 'code-command
       :mnemonic "ADD"
       :operands (list
		  (make-instance 'gpr-operand
		    :size 64
		    :code-number 2)
		  (make-instance 'immediate-operand
		    :value 33)))
     label1
     (make-instance 'code-command
       :mnemonic "ADD"
       :operands (list
		  (make-instance 'gpr-operand
		    :size 32
		    :code-number 3)
		  (make-instance 'immediate-operand
		    :value 44)))
     label2
     (make-instance 'code-command
       :mnemonic "JMP"
       :operands (list label1))
     (make-instance 'code-command
       :mnemonic "JNE"
       :operands (list label2)))))
     
     
   
