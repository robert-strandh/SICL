(in-package :x86-assembler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Some terminology.
;;;
;;; An OPERATION is an abstract concept.  Examples of operations are
;;; ADDITION, UNCONDITIONAL JUMP, RETURN FROM FUNCTION CALL, etc.  By
;;; definition, to each operation corresponds a unique MNEMONIC, such
;;; as ADD, MOV, CALL, etc.
;;;
;;; Each elementary step of a computation involves executing some
;;; operation with a sequence of OPERANDS.  An operand can be a
;;; REGISTER (General purpose, floating point, flags, etc), an
;;; EFFECTIVE ADDRESS denoting a sequence of locations in memory, or
;;; an IMMEDIATE operand.  Each operand has a SIZE associated with it
;;; which determines the number of bits involved in the operation.
;;; The different operands involved in a compuational step do not
;;; necessarily have the same size.  For instance, an 8-bit immedate
;;; can be used as a source operand when the target operand is a
;;; 64-bit general-purpose register.
;;;
;;; An INSTRUCTION is a sequence of between 1 and 15 bytes in memory
;;; that, when executed, will accomplish the intended operation with
;;; the operands that are given.  This sequence of bytes consists of
;;; some PREFIX bytes, one or two bytes of OPERATION CODE (or OPCODE
;;; for short), a ModRM byte, a SIB byte, a sequence (containing 1, 2,
;;; or 4 bytes) of bytes of DISPLACEMENT, and a sequence (containing
;;; 1, 2, or 4 bytes) of bytes of IMMEDIATES.  Only the OPCODE is
;;; mandatory.  The SIB byte may contain an OPCODE EXTENSION.
;;; Together, the opcode and the opcode extension (when present)
;;; determine the operation to be accomplished.
;;;
;;; For each operation, there are typically several different opcodes,
;;; depending on the operands involved in the elementatry computation
;;; step.
;;;
;;; The input to an assembler is called a SOURCE PROGRAM.  A source
;;; program contains a sequence of SOURCE ITEMS.  Each item can be a
;;; SYMBOLIC LABEL or a SOURCE COMMAND.  The purpose of a command is
;;; to instruct the assembler to emit a sequence of bytes.  The exact
;;; sequence of bytes that is emitted may depend on the other items in
;;; the program, and the position in the item with respect to other
;;; items.  Some commands instruct the assembler to emit instructions,
;;; and some other commands instruct it to emit DATA.
;;; 
;;; Step 1 of the assembler is to determine which source items are
;;; symbolic labels and which source items are commands.  Step 2 is to
;;; parse each source command into an ABSTRACT COMMAND.  When the
;;; command represents an instruction, the abstract command contains a
;;; complete description of the operation and operands involved,
;;; except that the exact values of some numeric values may not be
;;; completely known at this point.  The corresponding source command,
;;; on the other hand, may omit some of this information for the
;;; convenience of the assembly programmer.  Step 3 consists of
;;; computing the SIZE (i.e. the number of bytes that will be emitted)
;;; of each abstract command.  Step 4 assigns numeric values to
;;; symbolic labels, which will determine the missing numeric values
;;; of step 2.  Step 5 converts the abstract command to an INSTRUCTION
;;; or a sequence of DATA BYTES.  Finally, the resulting sequence of
;;; bytes is emitted.
;;;
;;; When the assembler is used in a backend of a compiler, steps 1 and
;;; 2 are unnecesssary.  It is therefore convenient to define a
;;; precise format for an assembly program represented as labels and
;;; abstract commands to be used by the compiler backend.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode of operation.
;;; 
;;; Either 64-bit or 32-bit for now.
;;;
;;; At the moment, we do not handle address-size overrides.  As a
;;; consequence, when the mode is 64-bit, base and index registers
;;; must be 64-bit registers, and when the mode is 32-bit, base and
;;; index registers must be 32-bit registers.

(defparameter *mode* '64-bit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Registers

(defparameter *32-bit-gprs*
  '(eax ecx edx ebx esp ebp esi edi))

(defparameter *64-bit-gprs*
  '(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

(defun register-name-p (object)
  (or (member object *32-bit-gprs*)
      (member object *64-bit-gprs*)))

(defun register-type (object)
  (assert (register-name-p object))
  (cond ((member object *32-bit-gprs*) 'reg32)
	((member object *64-bit-gprs*) 'reg64)
	(t nil)))

(defparameter *register-numbers*
  (let ((table (make-hash-table :test #'eq)))
    (loop for reg in *32-bit-gprs*
	  for i from 0
	  do (setf (gethash reg table) i))
    (loop for reg in *64-bit-gprs*
	  for i from 0
	  do (setf (gethash reg table) i))
    table))

(defun register-number (register)
  (gethash register *register-numbers*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Input.

;;; An INSTRUCTION LINE has the following form:
;;;
;;;   (<operation> <operand1> <operand2>)
;;;

;;; We allow for an effective address to take on one of the following
;;; forms:
;;;
;;;   * (<base-register>)
;;;     Base register only.
;;;
;;;   * (<base-register> <displacement>)
;;;     Base register and displacement.
;;;
;;;   * (<scale> <index-register> <base-register>)
;;;     Scale, index register, and base register.
;;;
;;;   * (<scale> <index-register> <base-register> <displacement>)
;;;     Scale, index register, base register, and displacement.

(defun form-modrm-byte (mod r/m reg)
  (declare (type (unsigned-byte 2) mod)
	   (type (unsigned-byte 3) r/m reg))
  (+ (ash mod 6) (ash reg 3) r/m))

(defun form-sib-byte (s i b)
  (declare (type (unsigned-byte 2) s)
	   (type (unsigned-byte 3) i b))
  (+ (ash s 6) (ash i 3) b))

(defun modrm-byte (reg effective-address)
  (ecase (length effective-address)
    (1
     (let* ((regno (register-number (car effective-address)))
	    (r/m (if (member (mod regno 8) '(4 5))
		     4
		     (mod regno 8))))
       (form-modrm-byte 0 reg r/m)))))

;;; Return true if and only if an effective address needs the SIB
;;; byte.


;;; Return 3 values: the S, I and B fields of the SIB byte.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operands.

(defun operand-type (operand)
  (cond ((listp operand)
	 'mem)
	((typep operand '(integer -128 127))
	 'imm8)
	((typep operand '(integer #.(- (expt 2 31)) #.(1- (expt 2 31))))
	 'imm32)
	((register-name-p operand)
	 (register-type operand))
	(t
	 (assert nil))))

(defun check-effective-address (effective-address)
  (let ((regtype (if (eq *mode* '64-bit) 'reg64 'reg32)))
    (assert (and (listp effective-address)
		 (case (length effective-address)
		   (1
		    (eq (register-type (first effective-address))
			regtype))
		   (2
		    (and (eq (register-type (first effective-address))
			     regtype)
			 (typep (second effective-address)
				'(signed-byte 32))))
		   (3
		    (and (typep (first effective-address)
				'(integer 1 8))
			 (eq (register-type (second effective-address))
			     regtype)
			 (not (member (second effective-address)
				      '(esp rsp)))
			 (eq (register-type (third effective-address))
			     regtype)))
		   (4
		    (and (typep (first effective-address)
				'(integer 1 8))
			 (eq (register-type (second effective-address))
			     regtype)
			 (not (member (second effective-address)
				      '(esp rsp)))
			 (eq (register-type (third effective-address))
			     regtype)
			 (typep (fourth effective-address)
				'(signed-byte 32)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defclass item () ())

(defclass label (item) ())

(defclass command (item) ())

(defclass code-command (command)
  ((%mnemonic :initarg :mnemonic :reader mnemonic)
   (%operands :initarg :operands :reader operands)))

(defclass data-command (command)
  ())

(defclass operand () ())

(defclass sized-operand (operand)
  ((%size :initarg :size :reader size)))

(defclass register-operand (sized-operand)
  ((%code-number :initarg :code-number :reader code-number)))

(defclass gpr-operand (register-operand)
  ())

(defclass mmx-register-operand (register-operand)
  ())

(defclass memory-operand (sized-operand)
  (;; An instance of gpr-operand, or NIL
   (%base-register
    :initform nil
    :initarg :base-register
    :reader base-register)
   ;; An instance of gpr-operand, or NIL
   (%index-register
    :initform nil
    :initarg :index-register
    :reader index-register)
   ;; 1, 2, 4, 8, or NIL
   (%scale
    :initform nil
    :initarg :scale
    :reader scale)
   ;; A signed integer or NIL.
   (%displacement
    :initform nil
    :initarg :displacement
    :reader displacement)))

(defun rex-p (memory-operand)
  (with-accessors ((base-register base-register)
		   (index-register index-register))
      memory-operand
    (or (and (not (null base-register))
	     (>= (code-number base-register) 8))
	(and (not (null index-register))
	     (>= (code-number index-register) 8)))))

(defun sib-byte-p (memory-operand)
  (with-accessors ((base-register base-register)
		   (index-register index-register))
      memory-operand
    (or
     ;; When there is an index register, there is always a SIB byte.
     (not (null index-register))
     (and (not (null base-register))
	  ;; If there is no index register, but there is a base
	  ;; register, then there is a SIB byte only if the R/M field
	  ;; is #b100.
	  (not (member (code-number base-register) '(4 12)))))))

(defun displacement-size (memory-operand)
  (with-accessors ((displacement displacement))
      memory-operand
    (cond ((null displacement)
	   0)
	  ((typep displacement '(signed-byte 8))
	   1)
	  (t
	   4))))

;;; We don't count the contribution of the REX prefix because we count
;;; that separately, in case the operand size requires a REX prefix as
;;; well.
(defun memory-operand-size (memory-operand)
  (+
   ;; The ModRM byte.
   1
   ;; The contribution of the SIB byte, if any.
   (if (sib-byte-p memory-operand) 1 0)
   ;; The contribution of the displacement.
   (displacement-size memory-operand)))

(defclass immediate-operand (operand)
  (;; A signed integer.
   (%value :initarg :value :reader value)))

(defun operand-matches-p (operand descriptor)
  (ecase (car descriptor)
    (gpr-a
     (and (typep operand 'gpr-operand)
	  (= (code-number operand) 0)
	  (= (size operand) (cadr descriptor))))
    (gpr
     (and (typep operand 'gpr-operand)
	  (= (size operand) (cadr descriptor))))
    (memory
     (and (typep operand 'memory-operand)
	  (= (size operand) (cadr descriptor))))
    (imm
     (and (typep operand 'immediate-operand)
	  (or (typep (value operand) `(signed-byte ,(cadr descriptor)))
	      (typep (value operand) `(unsigned-byte ,(cadr descriptor))))))
    (label
     ;; We don't take into account the size of the label at this
     ;; point, because we do not yet know what the final size of the
     ;; label is going to be.
     (typep operand 'label))))

(defun operands-match-p (operands descriptors)
  (and (= (length operands) (length descriptors))
       (every #'operand-matches-p operands descriptors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given an instruction descriptor and the operands to the command
;;; that the instruction descriptor matches, compute the size of the
;;; resulting instruction.

(defgeneric instruction-size-1 (desc opnd))

(defmethod instruction-size-1 (desc (opnd immediate-operand))
  (destructuring-bind (type size) (first (operands desc))
    (let* ((rex-p (rex.w desc))
	   (rex-contribution (if rex-p 1 0))
	   (override (operand-size-override desc))
	   (override-contribution (if override 1 0)))
      (ecase type
	((imm label)
	 (+ (length (opcodes desc))
	    (/ size 8)
	    rex-contribution
	    override-contribution))))))

(defmethod instruction-size-1 (desc (opnd gpr-operand))
  (destructuring-bind (type size) (first (operands desc))
    (declare (ignore size))
    (let* ((rex-p (or (rex.w desc) (>= (code-number opnd) 7)))
	   (rex-contribution (if rex-p 1 0))
	   (override (operand-size-override desc))
	   (override-contribution (if override 1 0)))
    (ecase type
      (modrm
       (+ (length (opcodes desc))
	  1 ; the ModRM byte
	  rex-contribution
	  override-contribution))))))

(defmethod instruction-size-1 (desc (opnd immediate-operand))
  (destructuring-bind (type size) (first (operands desc))
    (ecase type
      (imm
       (+ (length (opcodes desc)) (/ size 8))))))

(defmethod instruction-size-1 (desc (opnd memory-operand))
  (destructuring-bind (type size) (first (operands desc))
    (declare (ignore size))
    (let* ((rex-p (or (rex.w desc) (>= (code-number opnd) 7)))
	   (rex-contribution (if (or rex-p (rex-p opnd)) 1 0))
	   (override (operand-size-override desc))
	   (override-contribution (if override 1 0)))
      (ecase type
	(modrm
	 (+ (length (opcodes desc))
	    (memory-operand-size opnd)
	    rex-contribution
	    override-contribution))))))
       
(defgeneric instruction-size-2 (desc opnd1 opnd2))

(defmethod instruction-size-2
    (desc (opnd1 gpr-operand) (opnd2 immediate-operand))
  (let* ((rex-p (or (rex.w desc) (>= (code-number opnd1) 7)))
	 (rex-contribution (if rex-p 1 0))
	 (override (operand-size-override desc))
	 (override-contribution (if override 1 0)))
    (destructuring-bind (type1 size1) (first (operands desc))
      (declare (ignore size1))
      (destructuring-bind (type2 size2) (second (operands desc))
	(declare (ignore size2))
	(ecase type2
	  (imm
	   (+ (length (opcodes desc))
	      (ecase type1
		(modrm 1)
		(+r 0))
	      (etypecase (value opnd2)
		((or (unsigned-byte 1) (signed-byte 1)) 1)
		((or (unsigned-byte 2) (signed-byte 2)) 2)
		((or (unsigned-byte 4) (signed-byte 4)) 4))
	      rex-contribution
	      override-contribution)))))))

(defmethod instruction-size-2
  (desc (opnd1 gpr-operand) (opnd2 gpr-operand))
  (assert (or (equal (encoding desc) '(reg modrm))
	      (equal (encoding desc) '(modrm reg))))
  (let* ((rex-p (or (rex.w desc)
		    (>= (code-number opnd1) 7)
		    (>= (code-number opnd2) 7)))
	 (rex-contribution (if rex-p 1 0))
	 (override (operand-size-override desc))
	 (override-contribution (if override 1 0)))
    (+ (length (opcodes desc))
       1 ; There is always a ModRM byte
       rex-contribution
       override-contribution)))

(defmethod instruction-size-2
  (desc (opnd1 gpr-operand) (opnd2 memory-operand))
  (assert (equal (encoding desc) '(reg modrm)))
  (let* ((rex-p (or (rex.w desc)
		    (>= (code-number opnd1) 7)
		    (rex-p opnd2)))
	 (rex-contribution (if rex-p 1 0))
	 (override (operand-size-override desc))
	 (override-contribution (if override 1 0)))
    (+ (length (opcodes desc))
       (memory-operand-size opnd2)
       rex-contribution
       override-contribution)))

(defmethod instruction-size-2
  (desc (opnd1 memory-operand) (opnd2 immediate-operand))
  (assert (equal (encoding desc) '(modrm imm)))
  (let* ((rex-p (or (rex.w desc) (rex-p opnd2)))
	 (rex-contribution (if rex-p 1 0))
	 (override (operand-size-override desc))
	 (override-contribution (if override 1 0)))
    (+ (length (opcodes desc))
       (memory-operand-size opnd2)
       (etypecase (value opnd2)
	 ((or (unsigned-byte 1) (signed-byte 1)) 1)
	 ((or (unsigned-byte 2) (signed-byte 2)) 2)
	 ((or (unsigned-byte 4) (signed-byte 4)) 4))
       rex-contribution
       override-contribution)))

(defmethod instruction-size-2
  (desc (opnd1 memory-operand) (opnd2 gpr-operand))
  (assert (equal (encoding desc) '(modrm reg)))
  (let* ((rex-p (or (rex.w desc)
		    (>= (code-number opnd2) 7)
		    (rex-p opnd1)))
	 (rex-contribution (if rex-p 1 0))
	 (override (operand-size-override desc))
	 (override-contribution (if override 1 0)))
    (+ (length (opcodes desc))
       (memory-operand-size opnd1)
       rex-contribution
       override-contribution)))
  
(defun instruction-size (desc operands)
  (ecase (length operands)
    (1 (instruction-size-1 desc (first operands)))
    (2 (instruction-size-2 desc (first operands) (second operands)))))

;;; Take an item and return the preliminary size of that item. When
;;; the item is a label, the preliminary size is 0.  When the item is
;;; a CODE-COMMAND and it has a single operand of type LABEL, then the
;;; preliminary size is the MAXIMUM of the size of each candidate
;;; instruction.  When the item is a CODE-COMMAND and it has some
;;; other operands then the preliminary size is the MINIMUM of the
;;; size of each candidate instruction.
(defun preliminary-size (item)
  (cond ((typep item 'label)
	 0)
	((typep item 'data-command)
	 ;; We have no data commands right now
	 (error "can't handle data commands yet"))
	((typep item 'code-command)
	 (let* ((operands (operands item))
		(candidates (candidates (mnemonic item) operands)))
	   (reduce (if (and (= (length operands) 1)
			    (typep (first operands) 'label))
		       #'max
		       #'min)
		   (mapcar (lambda (desc)
			     (instruction-size desc operands))
			   candidates))))
	(t
	 (error "Item of unknown type: ~s" item))))

;;; From a list if items and a list of preliminary sizes, compute a
;;; dictionary (represented as a hash table) mapping items to
;;; preliminary absolute addresses from the beginning of the program.
(defun compute-preliminary-addresses (items preliminary-sizes)
  (loop with table = (make-hash-table :test #'eq)
	for absolute-address = 0 then (+ absolute-address size)
	for size in preliminary-sizes
	for item in items
	do (when (typep item 'label)
	     (setf (gethash item table) absolute-address))
	finally (return table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given an instruction descriptor and the operands to the command
;;; that the instruction descriptor matches, compute the encoding of
;;; the resulting instruction.

;;; Return a list of SIZE integers of type (UNSIGNED-BYTE 8) making up
;;; the representation of VALUE in a little-endian encoding, i.e., the
;;; bytes in the resulting list are ordered from least to most
;;; significant.
(defun encode-immediate (value size)
  (loop for position from 0 by 8
	repeat size
	collect (ldb (byte 8 position) value)))

(defgeneric encode-instruction-1 (desc opnd))

(defmethod encode-instruction-1 (desc (opnd immediate-operand))
  (destructuring-bind (type size) (first (operands desc))
    (let* ((rex-p (rex.w desc))
	   (override (operand-size-override desc)))
      (ecase type
	((imm label)
	 `(,@(cond (rex-p '(#x48))
		   (override '(#x66))
		   (t '()))
	   ,@(opcodes desc)
	   ,@(encode-immediate (value opnd) size)))))))
