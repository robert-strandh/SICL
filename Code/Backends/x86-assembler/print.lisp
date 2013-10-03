(cl:in-package #:x86-assembler)

(defparameter *regs-16*
  #("AX" "CX" "DX" "BX" "SP" "BP" "SI" "DI"
    "R8W" "R9W" "R10W" "R11W" "R12W" "R13W" "R14W" "R15W"))

(defparameter *regs-32*
  #("EAX" "ECX" "EDX" "EBX" "ESP" "EBP" "ESI" "EDI"
    "R8D" "R9D" "R10D" "R11D" "R12D" "R13D" "R14D" "R15D"))

(defparameter *regs-64*
  #("RAX" "RCX" "RDX" "RBX" "RSP" "RBP" "RSI" "RDI"
    "R8" "R9" "R10" "R11" "R12" "R13" "R14" "R15"))

(defun print-register (code size stream)
  (format stream "~a"
	  (aref (ecase size
		  (16 *regs-16*)
		  (32 *regs-32*)
		  (64 *regs-64*))
		code)))

(defgeneric print-item (item stream))

(defgeneric print-operand (operand stream))

(defmethod print-item ((item code-command) stream)
  (format stream "        ~a " (mnemonic item))
  (loop for operand in (butlast (operands item))
	do (print-operand operand stream)
	   (format stream ", "))
  (print-operand (car (last (operands item))) stream)
  (format stream "~%"))

(defmethod print-operand ((operand immediate-operand) stream)
  (format stream "~d" (value operand)))

(defmethod print-operand ((operand gpr-operand) stream)
  (print-register (code-number operand) (size operand) stream))

(defmethod print-operand ((operand memory-operand) stream)
  (format stream "[")
  (with-accessors ((base-register base-register)
		   (index-register index-register)
		   (scale scale)
		   (size size)
		   (displacement displacement))
      operand
    (cond ((and (null base-register) (null index-register))
	   (format stream "~d" displacement))
	  ((and (null index-register) (null displacement))
	   (print-register (base-register operand) size stream))
	  ((and (null base-register) (null displacement))
	   (format stream "~d*" scale)
	   (print-register (index-register operand) size stream))
	  ((null displacement)
	   (print-register (base-register operand) size stream)
	   (format stream "+")
	   (format stream "~d*" scale)
	   (print-register (index-register operand) size stream))
	  ((null index-register)
	   (print-register (base-register operand) size stream)
	   (format stream "~2d" displacement))
	  ((null base-register)
	   (format stream "~d*" scale)
	   (print-register (index-register operand) size stream)
	   (format stream "~@d" displacement))
	  (t
	   (print-register (base-register operand) size stream)
	   (format stream "+")
	   (format stream "~d*" scale)
	   (print-register (index-register operand) size stream)
	   (format stream "~@d" displacement))))
  (format stream "]"))

    
    
  
