(cl:in-package #:sicl-x86-64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The x86-64 backend.

;;; Registers can be catagorized according to several different
;;; criteria.  We exclue the stack pointer and the frame pointer from
;;; these categories.
;;;
;;; Some registers contain values that are potentially useful to the
;;; current function upon function entry.  These are the four
;;; registers that contain arguments, the register containing the
;;; argument count, the registers containing the static and the
;;; dynamic environments, and the register that contains the linkage
;;; rack.  The other registers do not contain any useful values.
;;;
;;; Some registers contain values that must be restored before
;;; returning to the caller and before making a tail call, because
;;; they contain values that the caller expect to be preserved.  These
;;; are the variable registers and the dynamic environment register.

(defvar *registers*
  (vector (sicl-mir:make-register-location 'rax)
	  (sicl-mir:make-register-location 'rbx)
	  (sicl-mir:make-register-location 'rcx)
	  (sicl-mir:make-register-location 'rdx)
	  (sicl-mir:make-register-location 'rsp)
	  (sicl-mir:make-register-location 'rbp)
	  (sicl-mir:make-register-location 'rsi)
	  (sicl-mir:make-register-location 'rdi)
	  (sicl-mir:make-register-location 'r8)
	  (sicl-mir:make-register-location 'r9)
	  (sicl-mir:make-register-location 'r10)
	  (sicl-mir:make-register-location 'r11)
	  (sicl-mir:make-register-location 'r12)
	  (sicl-mir:make-register-location 'r13)
	  (sicl-mir:make-register-location 'r14)
	  (sicl-mir:make-register-location 'r15)))

(defparameter *v1-reg* (aref *registers* 0))
(defparameter *denv-reg* (aref *registers* 1))
(defparameter *a4-v3-reg* (aref *registers* 2))
(defparameter *a3-v2-reg* (aref *registers* 3))
(defparameter *a2-v4-reg* (aref *registers* 6))
(defparameter *a1-vc-reg* (aref *registers* 7))
(defparameter *ac-vp-reg* (aref *registers* 8))
(defparameter *lv-fun-reg* (aref *registers* 9))
(defparameter *senv-reg* (aref *registers* 10))
(defparameter *scratch-reg* (aref *registers* 11))
(defparameter *var1-reg* (aref *registers* 12))
(defparameter *var2-reg* (aref *registers* 13))
(defparameter *var3-reg* (aref *registers* 14))
(defparameter *var4-reg* (aref *registers* 15))

;;; Indicate to the register allocator which registers are
;;; available.  For this backend, it is all registers except
;;; the stack pointer (RSP) and the frame pointer (RBP). 
(defmethod sicl-program:registers ((backend backend-x86-64))
  (loop for i in '(0 1 2 3 6 7 8 9 10 11 12 13 14 15)
	collect (aref *registers* i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Possibly convert MIR constants to immediates.
;;;
;;; FIXME: this code is not right.  We need to convert the constant
;;; input to a machine integer.  Furthermore, we can only convert
;;; fixnums this way.

(defmethod sicl-program:convert-constant ((backend backend-x86-64) constant)
  (cond ((and (typep constant 'sicl-mir:constant-input)
	      (typep (sicl-mir:value constant) 'integer))
	 ;; FIXME: compute the immediate from the fixnum in a better
	 ;; way, without assuming a particular representation.
	 (sicl-mir:make-immediate-input (* 4 (sicl-mir:value constant))))
	((typep constant 'sicl-mir:word-input)
	 (sicl-mir:make-immediate-input (sicl-mir:value constant)))
	(t
	 nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert a MIR instruction graph to LIR.

;;; Some registers have pre-determined lexicals into which the
;;; register must be spilled if spilling is required.  
(defvar *denv-lexical*)
(defvar *lv-lexical*)
(defvar *senv-lexical*)
(defvar *var1-lexical*)
(defvar *var2-lexical*)
(defvar *var3-lexical*)
(defvar *var4-lexical*)
(defvar *a1-lexical*)
(defvar *a2-lexical*)
(defvar *a3-lexical*)
(defvar *a4-lexical*)


(defgeneric convert-instruction (instruction))

(defmethod convert-instruction (instruction)
  (declare (ignore instruction))
  nil)

(defun convert-instruction-graph (initial-instruction)
  (let ((table (make-hash-table :test #'eq))
	;; Allocate temporaries for registers that might have to
	;; be saved. 
	(*denv-lexical* (sicl-mir:new-temporary))
	(*lv-lexical* (sicl-mir:new-temporary))
	(*senv-lexical* (sicl-mir:new-temporary))
	(*var1-lexical* (sicl-mir:new-temporary))
	(*var2-lexical* (sicl-mir:new-temporary))
	(*var3-lexical* (sicl-mir:new-temporary))
	(*var4-lexical* (sicl-mir:new-temporary))
	(*a1-lexical* (sicl-mir:new-temporary))
	(*a2-lexical* (sicl-mir:new-temporary))
	(*a3-lexical* (sicl-mir:new-temporary))
	(*a4-lexical* (sicl-mir:new-temporary))
	(all-instructions '()))
    (setf (sicl-program:required-register *denv-lexical*) *denv-reg*)
    (setf (sicl-program:required-register *lv-lexical*) *lv-fun-reg*)
    (setf (sicl-program:required-register *senv-lexical*) *senv-reg*)
    (setf (sicl-program:required-register *var1-lexical*) *var1-reg*)
    (setf (sicl-program:required-register *var2-lexical*) *var2-reg*)
    (setf (sicl-program:required-register *var3-lexical*) *var3-reg*)
    (setf (sicl-program:required-register *var4-lexical*) *var4-reg*)
    (setf (sicl-program:preferred-register *a1-lexical*) *a1-vc-reg*)
    (setf (sicl-program:preferred-register *a2-lexical*) *a2-v4-reg*)
    (setf (sicl-program:preferred-register *a3-lexical*) *a3-v2-reg*)
    (setf (sicl-program:preferred-register *a4-lexical*) *a4-v3-reg*)
    ;; Start by collecting all the instructions to be processed, so as
    ;; to avoid converting an instruction twice as the instruction
    ;; graph changes.
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (push instruction all-instructions)
		 (mapc #'traverse (sicl-mir:successors instruction)))))
      (traverse initial-instruction))
    ;; Now convert all instructions previously collected.
    (mapc #'convert-instruction all-instructions)))

(defmethod sicl-program:convert-to-lir
    ((backend backend-x86-64) initial-instruction)
  (convert-instruction-graph initial-instruction))

(defmethod convert-instruction ((instruction sicl-mir:enter-instruction))
  ;; Set the outputs of the ENTER instruction to all registers that
  ;; our caller may have put values in that are interesting to us.
  ;; That is the same as all register available to the register
  ;; allocator, except *v1-reg* and *scratch-reg*.
  (setf (sicl-mir:outputs instruction)
	(list *denv-reg* *a4-v3-reg* *a3-v2-reg* *a2-v4-reg*
	      *a1-vc-reg* *ac-vp-reg* *lv-fun-reg* *senv-reg*
	      *var1-reg* *var2-reg* *var3-reg* *var4-reg*))
  ;; Insert ASSIGNMENT instructions after the enter instruction.
  ;; These assignment instructions save the callee-saved registers, as
  ;; well as the static environment, and the linkage rack to their
  ;; corresponding lexical variables.
  (let ((saves (list (cons *denv-reg* *denv-lexical*)
		     (cons *lv-fun-reg* *lv-lexical*)
		     (cons *senv-reg* *senv-lexical*)
		     (cons *a4-v3-reg* *a4-lexical*)
		     (cons *a3-v2-reg* *a3-lexical*)
		     (cons *a2-v4-reg* *a2-lexical*)
		     (cons *a1-vc-reg* *a1-lexical*)
		     (cons *var4-reg* *var4-lexical*)
		     (cons *var3-reg* *var3-lexical*)
		     (cons *var2-reg* *var2-lexical*)
		     (cons *var1-reg* *var1-lexical*))))
    (loop for (register . lexical) in saves
	  for successor = (car (sicl-mir:successors instruction))
	  do (sicl-mir:insert-instruction-after
	      (sicl-mir:make-assignment-instruction register lexical)
	      instruction))))

;;; We only convert GET-ARG instructions that have immediate inputs
;;; that are small enough to correspond to a register argument. 
(defmethod convert-instruction ((instruction sicl-mir:get-arg-instruction))
  (let ((input (car (sicl-mir:inputs instruction))))
    (when (typep input 'sicl-mir:immediate-input)
      (let ((value (sicl-mir:value input)))
	(when (<= value 12)
	  (setf (sicl-mir:inputs instruction)
		(list (elt (list *a1-lexical* *a2-lexical*
				 *a3-lexical* *a4-lexical*)
			   (/ value 4))))
	  (change-class instruction 'sicl-mir:assignment-instruction))))))

(defmethod convert-instruction ((instruction sicl-mir:get-argcount-instruction))
  (change-class instruction 'sicl-mir:assignment-instruction)
  (setf (sicl-mir:inputs instruction)
	(list *ac-vp-reg*)))

(defmethod convert-instruction ((instruction sicl-mir:funcall-instruction))
  (let ((new-inputs (copy-list (sicl-mir:inputs instruction))))
    ;; Insert four assignment instructions preceding the
    ;; FUNCALL-INSTRUCTION, assigning lexical variables to the
    ;; physical registers used to store the corresponding arguments,
    ;; and replace the corresponding argument in the
    ;; FUNCALL-INSTRUCTION by the register.
    ;;
    ;; Recall that the first input is the function to be called, so
    ;; skip the first input when doing the assignment. 
    (loop for rest on (cdr new-inputs)
	  for reg in (list *a1-vc-reg* *a2-v4-reg* *a3-v2-reg* *a4-v3-reg*)
	  do (sicl-mir:insert-instruction-before
	      (sicl-mir:make-assignment-instruction (car rest) reg)
	      instruction)
	     (setf (car rest) reg))
    ;; Insert an assignment instruction that assigns the number of
    ;; arguments to the register *ac-vp-reg*.
    (sicl-mir:insert-instruction-before
     (sicl-mir:make-assignment-instruction
      ;; FIXME: do this better than to assume the representation of a
      ;; fixnum.
      (sicl-mir:make-immediate-input
       (* 4 (length (cdr new-inputs)))) *ac-vp-reg*)
     instruction)
    ;; Insert instructions for loading the callee into the register
    ;; dedicated for that purpose.
    (sicl-mir:insert-instruction-before
     (sicl-mir:make-assignment-instruction (car new-inputs) *lv-fun-reg*)
     instruction)
    ;; Add the linkage rack registers, and the argument count
    ;; register as input to the funcall instruction, right after the
    ;; callee input itself.
    (setf (sicl-mir:inputs instruction)
	  (cons (car new-inputs)
		(append (list *lv-fun-reg* *ac-vp-reg*) (cdr new-inputs))))
    ;; Indicate that the FUNCALL-INSTRUCTION returns values in some
    ;; registers, and trashes some others by adding those registers as
    ;; outputs to the FUNCALL-INSTRUCTION.
    (setf (sicl-mir:outputs instruction)
	  (list *a1-vc-reg* *a2-v4-reg* *a3-v2-reg* *a4-v3-reg* *ac-vp-reg*
		*scratch-reg* *v1-reg* *lv-fun-reg* *senv-reg*))))

(defmethod convert-instruction ((instruction sicl-mir:tailcall-instruction))
  (let ((new-inputs (copy-list (sicl-mir:inputs instruction))))
    ;; Insert four assignment instructions preceding the
    ;; TAILCALL-INSTRUCTION, assigning lexical variables to the
    ;; physical registers used to store the corresponding arguments,
    ;; and replace the corresponding argument in the
    ;; TAILCALL-INSTRUCTION by the register.
    ;;
    ;; Recall that the first input is the function to be called, so
    ;; skip the first input when doing the assignment. 
    (loop for rest on (cdr new-inputs)
	  for reg in (list *a1-vc-reg* *a2-v4-reg* *a3-v2-reg* *a4-v3-reg*)
	  do (sicl-mir:insert-instruction-before
	      (sicl-mir:make-assignment-instruction (car rest) reg)
	      instruction)
	     (setf (car rest) reg))
    ;; Insert an assignment instruction that assigns the number of
    ;; arguments to the register *ac-vp-reg*.
    (sicl-mir:insert-instruction-before
     (sicl-mir:make-assignment-instruction
      ;; FIXME: do this better than to assume the representation of a
      ;; fixnum.
      (sicl-mir:make-immediate-input
       (* 4 (length (cdr new-inputs)))) *ac-vp-reg*)
     instruction)
    ;; Insert instructions for loading the callee into the register
    ;; dedicated for that purpose.
    (sicl-mir:insert-instruction-before
     (sicl-mir:make-assignment-instruction (car new-inputs) *lv-fun-reg*)
     instruction)
    ;; Add the linkage rack registers, and the argument count
    ;; register as input to the funcall instruction, right after the
    ;; callee input itself.
    (setf (sicl-mir:inputs instruction)
	  (cons (car new-inputs)
		(append (list *lv-fun-reg* *ac-vp-reg*) (cdr new-inputs)))))
  ;; The tailcall instruction is a combination of a funcall and a
  ;; return instruction.  For that reason, we must also restore the
  ;; callee-saved registers just the way we do with a return
  ;; instruction.
  (let ((saves (list (cons *denv-reg* *denv-lexical*)
		     (cons *var4-reg* *var4-lexical*)
		     (cons *var3-reg* *var3-lexical*)
		     (cons *var2-reg* *var2-lexical*)
		     (cons *var1-reg* *var1-lexical*))))
    (loop for (register . lexical) in saves
	  do (sicl-mir:insert-instruction-before
	      (sicl-mir:make-assignment-instruction lexical register)
	      instruction)))
  ;;Add the callee-saved registers as additional inputs to the return
  ;;instruction.
  (setf (sicl-mir:inputs instruction)
	(append (list *denv-reg* *var4-reg* *var3-reg* *var2-reg* *var1-reg*)
		(sicl-mir:inputs instruction))))

;;; For the ENCLOSE-INSTRUCTION, we just add the static environment
;;; and the linkage rack as inputs to the instruction so that the
;;; register allocator can do its thing.
(defmethod convert-instruction ((instruction sicl-mir:enclose-instruction))
  (convert-instruction-graph (car (sicl-mir:inputs instruction)))
  (push *senv-lexical* (sicl-mir:inputs instruction))
  (push *lv-lexical* (sicl-mir:inputs instruction)))

(defmethod convert-instruction ((instruction sicl-mir:get-values-instruction))
  (let ((new-outputs (copy-list (sicl-mir:outputs instruction))))
    (loop for rest on new-outputs
	  for reg in (list *v1-reg* *a3-v2-reg*
			   *a4-v3-reg* *a2-v4-reg*)
	  do (sicl-mir:insert-instruction-after
	      (sicl-mir:make-assignment-instruction reg (car rest))
	      instruction)
	     (setf (car rest) reg))
    (setf (sicl-mir:outputs instruction) new-outputs)))

(defmethod convert-instruction ((instruction sicl-mir:return-instruction))
  (let ((new-inputs (copy-list (sicl-mir:inputs instruction))))
    ;; Replace the input by the corresponding register.  
    (loop for rest on new-inputs
	  for reg in (list *v1-reg* *a3-v2-reg* *a4-v3-reg* *a2-v4-reg*)
	  do (sicl-mir:insert-instruction-before
	      (sicl-mir:make-assignment-instruction (car rest) reg)
	      instruction)
	     (setf (car rest) reg))
    ;; Add an assignment instruction, assigning the value count to the
    ;; *a1-vc-reg* register.
    (sicl-mir:insert-instruction-before
     (sicl-mir:make-assignment-instruction
      ;; FIXME: do this better than to assume the representation of a
      ;; fixnum.
      (sicl-mir:make-immediate-input (* 4 (length new-inputs)))
      *a1-vc-reg*)
     instruction)
    ;; Add the *a1-vc-reg* register as another input to the return
    ;; instruction.
    (push *ac-vp-reg* new-inputs)
    ;; Assign the modified inputs list to the instruction inputs.
    (setf (sicl-mir:inputs instruction) new-inputs))
  ;; Restore the callee-saved registers from their corresponding
  ;; lexical variables.
  (let ((saves (list (cons *denv-reg* *denv-lexical*)
		     (cons *var4-reg* *var4-lexical*)
		     (cons *var3-reg* *var3-lexical*)
		     (cons *var2-reg* *var2-lexical*)
		     (cons *var1-reg* *var1-lexical*))))
    (loop for (register . lexical) in saves
	  do (sicl-mir:insert-instruction-before
	      (sicl-mir:make-assignment-instruction lexical register)
	      instruction)))
  ;;Add the callee-saved registers as additional inputs to the return
  ;;instruction.
  (setf (sicl-mir:inputs instruction)
	(append (list *denv-reg* *var4-reg* *var3-reg* *var2-reg* *var1-reg*)
		(sicl-mir:inputs instruction))))

(defmethod convert-instruction
    ((instruction sicl-mir:load-constant-instruction))
  (let ((lv-index (sicl-mir:linkage-vector-index instruction)))
    (change-class instruction 'sicl-mir:memref-instruction
		  :displacement (* 8 (1+ lv-index))))
  (push *lv-lexical* (sicl-mir:inputs instruction)))

(defmethod convert-instruction
    ((instruction sicl-mir:load-global-instruction))
  (let* ((lv-index (sicl-mir:linkage-vector-index instruction))
	 (temp (sicl-mir:new-temporary))
	 (new (sicl-mir:make-memref-instruction
	       *lv-lexical*
	       (* 8 (1+ lv-index))
	       temp
	       instruction)))
    (change-class instruction 'sicl-mir:memref-instruction
		  :displacement -1)
    (sicl-mir:insert-instruction-before new instruction)
    (setf (sicl-mir:inputs instruction)
	  (list temp))))
