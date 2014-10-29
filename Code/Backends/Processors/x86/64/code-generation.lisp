(cl:in-package #:sicl-x86-64)

;;; Define a correspondance bettween MIR register locations and
;;; register operands as the assembler wants.

(defparameter *regs*
  `((rax . ,(x86-assembler:make-gpr-operand 64 0))
    (rcx . ,(x86-assembler:make-gpr-operand 64 1))
    (rdx . ,(x86-assembler:make-gpr-operand 64 2))
    (rbx . ,(x86-assembler:make-gpr-operand 64 3))
    (rsp . ,(x86-assembler:make-gpr-operand 64 4))
    (rbp . ,(x86-assembler:make-gpr-operand 64 5))
    (rsi . ,(x86-assembler:make-gpr-operand 64 6))
    (rdi . ,(x86-assembler:make-gpr-operand 64 7))
    (r8 . ,(x86-assembler:make-gpr-operand 64 8))
    (r9 . ,(x86-assembler:make-gpr-operand 64 9))
    (r10 . ,(x86-assembler:make-gpr-operand 64 10))
    (r11 . ,(x86-assembler:make-gpr-operand 64 11))
    (r12 . ,(x86-assembler:make-gpr-operand 64 12))
    (r13 . ,(x86-assembler:make-gpr-operand 64 13))
    (r14 . ,(x86-assembler:make-gpr-operand 64 14))
    (r15 . ,(x86-assembler:make-gpr-operand 64 15))))

(defun make-reg (register)
  (let ((reg-name (if (symbolp register)
		      register
		      (sicl-mir:name register))))
    (cdr (assoc reg-name *regs*))))

(defun make-imm (immediate-location)
  (x86-assembler:make-immediate-operand (sicl-mir:value immediate-location)))

(defun make-mem (register displacement)
  (let ((reg-name (if (symbolp register)
		      register
		      (sicl-mir:name register))))
    (x86-assembler:make-memory-operand
     64
     :base-register (position reg-name *regs* :test #'eq :key #'car)
     :displacement displacement)))

(defvar *assembly-program*)

(defun add-item (item)
  (push item *assembly-program*))

(defun make-code (mnemonic operands)
  (x86-assembler:make-code-command mnemonic operands))

(defun add-code (mnemonic operands)
  (add-item (make-code mnemonic operands)))

(defvar *label-table*)

(defun assign-label (instruction)
  (unless (gethash instruction *label-table*)
    (setf (gethash instruction *label-table*)
	  (x86-assembler:make-label))))

(defvar *processed-p-table*)

(defgeneric generate-instruction (instruction stream))

(defmethod generate-instruction :around (instruction stream)
  (if (gethash instruction *processed-p-table*)
      (add-code "JMP" (list (gethash instruction *label-table*)))
      (call-next-method)))

(defmethod generate-instruction :before (instruction stream)
  (when (> (length (sicl-mir:predecessors instruction)) 1)
    (assign-label instruction))
  (let ((label (gethash instruction *label-table*)))
    (unless (null label)
      (add-item label)))
  (setf (gethash instruction *processed-p-table*) t))

(defmethod generate-instruction :after (instruction stream)
  (let ((successors (sicl-mir:successors instruction)))
    (unless (null successors)
      (generate-instruction (car successors) stream))
      (unless (null (cdr successors))
	(unless (gethash (cadr successors) *processed-p-table*)
	  (generate-instruction (cadr successors) stream)))))
	    
(defmethod generate-instruction
    ((instruction sicl-mir:s<-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input2) (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JL" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JL" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP" (list (make-reg input1) (make-imm input2)))
	  (assign-label (cadr successors))
	  (add-code "JNL" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input1) (make-reg input2)))
	  (assign-label (cadr successors))
	  (add-code "JNL" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-reg input1)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNL" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))
			  (make-imm input2)))
	  (assign-label (cadr successors))
	  (add-code "JNL" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))
			  (make-reg input2)))
	  (assign-label (cadr successors))
	  (add-code "JNL" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))))
	  (add-code "CMP"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNL" (list (gethash (cadr successors) *label-table*)))))))))

(defmethod generate-instruction
    ((instruction sicl-mir:s<=-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input2) (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JLE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JLE" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP" (list (make-reg input1) (make-imm input2)))
	  (assign-label (cadr successors))
	  (add-code "JNLE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input1) (make-reg input2)))
	  (assign-label (cadr successors))
	  (add-code "JNLE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-reg input1)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNLE" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JNLE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-reg input1)))
	  (assign-label (cadr successors))
	  (add-code "JNLE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))))
	  (add-code "CMP"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNLE" (list (gethash (cadr successors) *label-table*)))))))))

(defmethod generate-instruction
    ((instruction sicl-mir:u<-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input2) (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JB" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JB" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP" (list (make-reg input1) (make-imm input2)))
	  (assign-label (cadr successors))
	  (add-code "JNB" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input1) (make-reg input2)))
	  (assign-label (cadr successors))
	  (add-code "JNB" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-reg input1)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNB" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JNB" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-reg input1)))
	  (assign-label (cadr successors))
	  (add-code "JNB" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))))
	  (add-code "CMP"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNB" (list (gethash (cadr successors) *label-table*)))))))))

(defmethod generate-instruction
    ((instruction sicl-mir:u<=-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input2) (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JBE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JBE" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP" (list (make-reg input1) (make-imm input2)))
	  (assign-label (cadr successors))
	  (add-code "JNBE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input1) (make-reg input2)))
	  (assign-label (cadr successors))
	  (add-code "JNBE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-reg input1)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNBE" (list (gethash (cadr successors) *label-table*))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1)))
	  (assign-label (cadr successors))
	  (add-code "JNBE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:register-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-reg input1)))
	  (assign-label (cadr successors))
	  (add-code "JNBE" (list (gethash (cadr successors) *label-table*))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))))
	  (add-code "CMP"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))
	  (assign-label (cadr successors))
	  (add-code "JNBE" (list (gethash (cadr successors) *label-table*)))))))))

(defmethod generate-instruction
    ((instruction sicl-mir:==-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input2) (make-imm input1))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP" (list (make-reg input1) (make-imm input2))))
	 (sicl-mir:register-location
	  (add-code "CMP" (list (make-reg input1) (make-reg input2))))
	 (sicl-mir:dynamic-location
	  (add-code "CMP"
		    (list (make-reg input1)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2)))))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-imm input1))))
	 (sicl-mir:register-location
	  (add-code "CMP"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))
			  (make-reg input1))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input1))))))
	  (add-code "CMP"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input2))))))))))
    (assign-label (cadr successors))
    (add-code "JNE" (list (gethash (cadr successors) *label-table*)))))

(defmethod generate-instruction
    ((instruction sicl-mir:enter-instruction) stream)
  ;; For now, don't generate any code.  Later, this is where we
  ;; allocate stack space for dynamic locations. 
  (declare (ignore stream))
  nil)

(defmethod generate-instruction
    ((instruction sicl-mir:return-instruction) stream)
  (add-code "RET" '()))

(defmethod generate-instruction
    ((instruction sicl-mir:funcall-instruction) stream)
  ;; When we come here, the function object is already contained in
  ;; *lv-fun-reg*.  The end result we want is that the linkage rack
  ;; be in *lv-fun-reg* and that we issue a CALL instruction to the
  ;; entry point of the function.
  (let ((name (sicl-mir:name *lv-fun-reg*)))
    ;; Start by loading the rack of the function object to
    ;; *lv-fun-reg*.
    (add-code "MOV" (list (make-reg name) (make-mem name 1)))
    ;; Next, load the entry point address to RAX.
    ;; FIXME: check the offset of the entry point.
    ;; FIXME: generate the offset of the entry point automatically.
    (add-code "MOV" (list (make-reg 'rax) (make-mem name 40)))
    ;; Next, load the static environment into *senv-reg*.
    ;; FIXME: check the offset of the static environment.
    ;; FIXME: generate the offset of the static environment automatically.
    (add-code "MOV" (list (make-reg *senv-reg*) (make-mem name 24)))
    ;; Next, load the linkage rack into *lv-fun-reg*.
    ;; FIXME: check the offset of the linkage rack.
    ;; FIXME: generate the offset of the linkage rack automatically.
    (add-code "MOV" (list (make-reg name) (make-mem name 32)))
    ;; Finally, call the entry point.
    (add-code "CALL" (list (make-reg 'rax)))))

(defmethod generate-instruction
    ((instruction sicl-mir:tailcall-instruction) stream)
  (let ((name (sicl-mir:name *lv-fun-reg*)))
    ;; Start by loading the rack of the function object to
    ;; *lv-fun-reg*.
    (add-code "MOV" (list (make-reg name) (make-mem name 1)))
    ;; Next, load the entry point address to RAX.
    ;; FIXME: check the offset of the entry point.
    ;; FIXME: generate the offset of the entry point automatically.
    (add-code "MOV" (list (make-reg 'rax) (make-mem name 40)))
    ;; Next, load the static environment into *senv-reg*.
    ;; FIXME: check the offset of the static environment.
    ;; FIXME: generate the offset of the static environment automatically.
    (add-code "MOV" (list (make-reg *senv-reg*) (make-mem name 24)))
    ;; Next, load the linkage rack into *lv-fun-reg*.
    ;; FIXME: check the offset of the linkage rack.
    ;; FIXME: generate the offset of the linkage rack automatically.
    (add-code "MOV" (list (make-reg name) (make-mem name 32)))
    ;; Finally, jump to the entry point.
    (add-code "JMP" (list (make-reg 'rax)))))

(defmethod generate-instruction
    ((instruction sicl-mir:assignment-instruction) stream)
  (let ((input (first (sicl-mir:inputs instruction)))
	(output (first (sicl-mir:outputs instruction))))
    (etypecase input
      (sicl-mir:immediate-input
       (etypecase output
	 (sicl-mir:register-location
	  (add-code "MOV" (list (make-reg output) (make-imm input))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index output))))
			  (make-imm input))))))
      (sicl-mir:register-location
       (etypecase output
	 (sicl-mir:register-location
	  (add-code "MOV" (list (make-reg output) (make-reg input))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index output))))
			  (make-reg input))))))
      (sicl-mir:dynamic-location
       (etypecase output
	 (sicl-mir:register-location
	  (add-code "MOV"
		    (list (make-reg output)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input)))))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list  (make-reg 'rax)
			   (make-mem 'rbp (* -8 (1+ (sicl-mir:index input))))))
	  (add-code "MOV"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index output))))
			  (make-reg 'rax)))))))))

(defmethod generate-instruction
    ((instruction sicl-mir:memref-instruction) stream)
  (let ((input (first (sicl-mir:inputs instruction)))
	(displacement (sicl-mir:displacement instruction))
	(output (first (sicl-mir:outputs instruction))))
    (etypecase input
      (sicl-mir:register-location
       (etypecase output
	 (sicl-mir:register-location
	  (add-code "MOV"
		    (list (make-reg output)
			  (make-mem input displacement))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem input displacement)))
	  (add-code "MOV"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index output))))
			  (make-reg 'rax))))))
      (sicl-mir:dynamic-location
       (etypecase output
	 (sicl-mir:register-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input))))))
	  (add-code "MOV"
		    (list (make-reg output)
			  (make-mem 'rax displacement))))
	 (sicl-mir:dynamic-location
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rbp (* -8 (1+ (sicl-mir:index input))))))
	  (add-code "MOV"
		    (list (make-reg 'rax)
			  (make-mem 'rax displacement)))
	  (add-code "MOV"
		    (list (make-mem 'rbp (* -8 (1+ (sicl-mir:index output))))
			  (make-reg 'rax)))))))))

(defun generate-code (first-instruction)
  (let ((*label-table* (make-hash-table :test #'eq))
	(*processed-p-table* (make-hash-table :test #'eq))
	(*assembly-program* '()))
    (generate-instruction first-instruction *standard-output*)
    (x86-assembler:assemble (reverse *assembly-program*))))
      
