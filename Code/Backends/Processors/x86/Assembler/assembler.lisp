(cl:in-package :x86-assembler)

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
;;; mandatory.  The ModRM byte may contain an OPCODE EXTENSION.
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
;;; Items of the list making up a source program.

(defclass item () ())

(defclass label (item) ())

(defun make-label ()
  (make-instance 'label))

(defclass command (item) ())

(defclass code-command (command)
  ((%mnemonic :initarg :mnemonic :reader mnemonic)
   (%operands :initarg :operands :reader operands)))

(defun make-code-command (mnemonic operands)
  (make-instance 'code-command
    :mnemonic mnemonic
    :operands operands))

(defclass data-command (command)
  ())

(defclass operand () ())

(defclass sized-operand (operand)
  ((%size :initarg :size :reader size)))

(defclass register-operand (sized-operand)
  ((%code-number :initarg :code-number :reader code-number)))

(defclass gpr-operand (register-operand)
  ())

(defun make-gpr-operand (size code-number)
  (make-instance 'gpr-operand
    :size size
    :code-number code-number))

(defclass mmx-register-operand (register-operand)
  ())

(defclass memory-operand (sized-operand)
  (;; An integer or NIL.
   (%base-register
    :initform nil
    :initarg :base-register
    :reader base-register)
   ;; An integer or NIL
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

(defun make-memory-operand
    (size &key base-register index-register scale displacement)
  (make-instance 'memory-operand
    :size size
    :base-register base-register
    :index-register index-register
    :scale scale
    :displacement displacement))

(defclass immediate-operand (operand)
  (;; A signed integer.
   (%value :initarg :value :reader value)))

(defun make-immediate-operand (value)
  (make-instance 'immediate-operand
    :value value))

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
    (simm
     (and (typep operand 'immediate-operand)
	  (typep (value operand) `(signed-byte ,(cadr descriptor)))))
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
;;; that the instruction descriptor matches, compute the encoding of
;;; the resulting instruction.

;;; Return a list of SIZE integers of type (UNSIGNED-BYTE 8) making up
;;; the representation of VALUE in a little-endian encoding, i.e., the
;;; bytes in the resulting list are ordered from least to most
;;; significant.
(defun encode-integer (value size)
  (loop for position from 0 by 8
	repeat size
	collect (ldb (byte 8 position) value)))

(defgeneric encode-instruction-1 (desc opnd))

(defmethod encode-instruction-1 (desc (opnd immediate-operand))
  (let ((type (first (encoding desc)))
	(length (/ (second (first (operands desc))) 8)))
    (ecase type
      (imm
       (let* ((rex-p (rex.w desc)))
	 `(,@(if (operand-size-override desc) '(#x66) '())
	   ,@(if rex-p '(#x48) '())
	   ,@(opcodes desc)
	   ,@(encode-integer (value opnd) length)))))))

;;; A hash table mapping items to addresses relative to the
;;; beginning of the program.
(defparameter *addresses* nil)

;;; The address (relative to the beginning of the program) of the
;;; instruction immediately following the one being encoded.
(defparameter *instruction-pointer* nil)

(defmethod encode-instruction-1 (desc (opnd label))
  (let ((type (first (encoding desc))))
    (ecase type
      (label
       (let* ((rex-p (rex.w desc)))
	 `(,@(if (operand-size-override desc) '(#x66) '())
	   ,@(if rex-p '(#x48) '())
	   ,@(opcodes desc)
	   ,@(encode-integer (- (gethash opnd *addresses*)
				*instruction-pointer*)
			     4)))))))

(defmethod encode-instruction-1 (desc (opnd gpr-operand))
  (let ((type (first (encoding desc))))
    (ecase type
      (modrm
       `(,@(if (operand-size-override desc) '(#x66) '())
	 ,@(if (rex.w desc)
	       (if (>= (code-number opnd) 7)
		   '(#b01001001)
		   '(#b01001000))
	       (if (>= (code-number opnd) 7)
		   '(#b01000001)
		   '()))
	 ,@(opcodes desc)
	 ,(+ #b11000000
	     (ash (opcode-extension desc) 3)
	     (mod (code-number opnd) 8)))))))

;;; Always include the RXB bits of a potential REX byte.
(defun encode-memory-operand (memory-operand)
  (with-accessors ((base-register base-register)
		   (index-register index-register)
		   (scale scale)
		   (displacement displacement))
      memory-operand
    (cond ((and (null base-register)
		(null index-register))
	   ;; We have only a displacement.
	   `(#b000
	     #b00000101
	     ,@(encode-integer displacement 4)))
	  ((and (null index-register)
		(null displacement))
	   ;; We have only a base register.
	   (multiple-value-bind (rex.b r/m)
	       (floor base-register 8)
	     (if (= r/m 4)
		 `(,rex.b
		   #b00000100  ; ModR/M byte
		   #b00100100) ; SIB byte
		 `(,rex.b
		   ,r/m))))
	  ((and (null index-register)
		(typep displacement '(signed-byte 8)))
	   ;; We have a base register and an 8-bit displacement.
	   (multiple-value-bind (rex.b r/m)
	       (floor base-register 8)
	     (if (= r/m 4)
		 `(,rex.b
		   #b01000100 ; ModR/M byte
		   #b00100100 ; SIB byte
		   ,(encode-integer displacement 1))
		 `(,rex.b
		   ,(+ #b01000000 r/m)
		   ,@(encode-integer displacement 1)))))
	  ((and (null index-register)
		(typep displacement '(signed-byte 32)))
	   ;; We have a base register and a 32-bit displacement.
	   (multiple-value-bind (rex.b r/m)
	       (floor base-register 8)
	     (if (= r/m 4)
		 `(,rex.b
		   #b10000100 ; ModR/M byte
		   #b00100100 ; SIB byte
		   ,(encode-integer displacement 4))
		 `(,rex.b
		   ,(+ #b10000000 r/m)
		   ,@(encode-integer displacement 4)))))
	  ((null base-register)
	   ;; The only encoding provided when there is no base
	   ;; register has a 32-bit displacement, so even if the
	   ;; displacement is small or even 0, we must use this
	   ;; encoding.
	   (multiple-value-bind (rex.x i)
	       (floor index-register 8)
	     `(,(ash rex.x 1)
	       #b00000100 ; ModR/M byte
	       ,(+ (ash (round (log scale 2)) 6)
		   (ash i 3)
		   #b101)
	       ,@(encode-integer (or displacement 0) 4))))
	  ((null displacement)
	   (multiple-value-bind (rex.b b)
	       (floor base-register 8)
	     (multiple-value-bind (rex.x i)
		 (floor index-register 8)
	       (if (= b 5)
		   ;; If the base register is 5 (EBP) or 13, then we
		   ;; have a problem, because there is no encoding for
		   ;; that situation without a displacement.  So we
		   ;; use a displacement of 0.
		   `(,(+ (ash rex.x 1) rex.b)
		     #b01000100 ; ModR/M byte
		     ,(+ (ash (round (log scale 2)) 6)
			 (ash i 3)
			 b)
		     0)
		   `(,(+ (ash rex.x 1) rex.b)
		     #b00000100 ; ModR/M byte
		     ,(+ (ash (round (log scale 2)) 6)
			 (ash i 3)
			 b))))))
	  (t
	   (multiple-value-bind (rex.b b)
	       (floor base-register 8)
	     (multiple-value-bind (rex.x i)
		 (floor index-register 8)
	       (if (typep displacement '(signed-byte 8))
		   `(,(+ (ash rex.x 1) rex.b)
		     #b01000100 ; ModR/M byte
		     ,(+ (ash (round (log scale 2)) 6)
			 (ash i 3)
			 b)
		     ,@(encode-integer displacement 1))
		   `(,(+ (ash rex.x 1) rex.b)
		     #b10000100 ; ModR/M byte
		     ,(+ (ash (round (log scale 2)) 6)
			 (ash i 3)
			 b)
		     ,@(encode-integer displacement 4)))))))))

(defmethod encode-instruction-1 (desc (opnd memory-operand))
  (let ((type (first (encoding desc))))
    (ecase type
      (modrm
       (destructuring-bind (rex.xb modrm &rest rest)
	   (encode-memory-operand opnd)
	 (let ((rex-low (+ (if (rex.w opnd) #b1000 0) rex.xb)))
	   `(,@(if (operand-size-override desc) '(#x66) '())
	     ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
	     ,@(opcodes desc)
	     ,(logior modrm (ash (opcode-extension opnd) 3))
	     ,@rest)))))))

(defgeneric encode-instruction-2 (desc opnd1 opnd2))

(defmethod encode-instruction-2
    (desc (opnd1 gpr-operand) (opnd2 immediate-operand))
  (multiple-value-bind (rex.b r/m)
      (floor (code-number opnd1) 8)
    (let* ((rex-low (+ (if (rex.w desc) #b1000 0) rex.b)))
      (let ((type1 (first (encoding desc)))
	    (type2 (second (encoding desc)))
	    (length2 (/ (second (second (operands desc))) 8)))
	(ecase type1
	  (-
	   (ecase type2
	     (imm
	      `(,@(if (operand-size-override desc) '(#x66) '())
		,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
		,@(opcodes desc)
		,@(encode-integer (value opnd2) length2)))))
	  (modrm
	   (ecase type2
	     (imm
	      `(,@(if (operand-size-override desc) '(#x66) '())
		,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
		,@(opcodes desc)
		,(+ (ash #b11 6)
		    (ash (opcode-extension desc) 3)
		    r/m)
		,@(encode-integer (value opnd2) length2)))))
	  (+r
	   (ecase type2
	     (imm
	      `(,@(if (operand-size-override desc) '(#x66) '())
		,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
		,(+ (car (opcodes desc)) r/m)
		,@(encode-integer (value opnd2) length2))))))))))

(defmethod encode-instruction-2
  (desc (opnd1 gpr-operand) (opnd2 gpr-operand))
  (assert (or (equal (encoding desc) '(reg modrm))
	      (equal (encoding desc) '(modrm reg))))
  (when (equal (encoding desc) '(modrm reg))
    (rotatef opnd1 opnd2))
  (multiple-value-bind (rex.b r/m)
      (floor (code-number opnd2) 8)
    (multiple-value-bind (rex.r reg)
	(floor (code-number opnd1) 8)
      (let ((rex-low (+ (if (rex.w desc) #b1000 0)
			(ash rex.r 2)
			rex.b)))
	`(,@(if (operand-size-override desc) '(#x66) '())
	  ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
	  ,@(opcodes desc)
	  ,(+ #b11000000
	      (ash reg 3)
	      r/m))))))

(defmethod encode-instruction-2
  (desc (opnd1 gpr-operand) (opnd2 memory-operand))
  (assert (equal (encoding desc) '(reg modrm)))
  (destructuring-bind (rex.xb modrm &rest rest)
      (encode-memory-operand opnd2)
    (multiple-value-bind (rex.r reg)
	(floor (code-number opnd1) 8)
      (let ((rex-low (+ (if (rex.w desc) #b1000 0)
			rex.xb
			(ash rex.r 2))))
	`(,@(if (operand-size-override desc) '(#x66) '())
	  ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
	  ,@(opcodes desc)
	  ,(logior modrm (ash reg 3))
	  ,@rest)))))
		      
(defmethod encode-instruction-2
  (desc (opnd1 memory-operand) (opnd2 immediate-operand))
  (assert (equal (encoding desc) '(modrm imm)))
  (destructuring-bind (rex.xb modrm &rest rest)
      (encode-memory-operand opnd1)
    (let ((rex-low (+ (if (rex.w desc) #b1000 0) rex.xb)))
      `(,@(if (operand-size-override desc) '(#x66) '())
	,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
	,@(opcodes desc)
	,(logior modrm (ash (opcode-extension desc) 3))
	,@rest
	,@(encode-integer (value opnd2)
			  (/ (second (second (operands desc))) 8))))))

(defmethod encode-instruction-2
  (desc (opnd1 memory-operand) (opnd2 gpr-operand))
  (assert (equal (encoding desc) '(modrm reg)))
  (destructuring-bind (rex.xb modrm &rest rest)
      (encode-memory-operand opnd1)
    (multiple-value-bind (rex.r reg)
	(floor (code-number opnd2) 8)
      (let ((rex-low (+ (if (rex.w desc) #b1000 0)
			rex.xb
			(ash rex.r 2))))
	`(,@(if (operand-size-override desc) '(#x66) '())
	  ,@(if (plusp rex-low) `(,(+ #x40 rex-low)) '())
	  ,@(opcodes desc)
	  ,(logior modrm (ash reg 3))
	  ,@rest)))))

(defun encode-instruction (desc operands)
  (ecase (length operands)
    (0 (opcodes desc))
    (1 (encode-instruction-1 desc (first operands)))
    (2 (encode-instruction-2 desc (first operands) (second operands)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Given an instruction descriptor and the operands to the command
;;; that the instruction descriptor matches, compute the size of the
;;; resulting instruction.

(defgeneric instruction-size-1 (desc opnd))

(defmethod instruction-size-1 (desc opnd)
  (length (encode-instruction-1 desc opnd)))

(defmethod instruction-size-1 (desc (opnd label))
  (let ((type (first (encoding desc))))
    (ecase type
      (label
       (+ (if (operand-size-override desc) 1 0)
	  (if (rex.w desc) 1 0)
	  (length (opcodes desc))
	  (/ (second (first (operands desc))) 8))))))

(defgeneric instruction-size-2 (desc opnd1 opnd2))

(defmethod instruction-size-2 (desc opnd1 opnd2)
  (length (encode-instruction-2 desc opnd1 opnd2)))
  
(defun instruction-size (desc operands)
  (ecase (length operands)
    (0 (length (opcodes desc)))
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
;;; preliminary addresses relative to the beginning of the program.
(defun compute-preliminary-addresses (items preliminary-sizes)
  (loop with table = (make-hash-table :test #'eq)
	for absolute-address = 0 then (+ absolute-address size)
	for size in preliminary-sizes
	for item in items
	do (when (typep item 'label)
	     (setf (gethash item table) absolute-address))
	finally (return table)))

(defun compute-encoding (item)
  (cond ((typep item 'label)
	 '())
	((typep item 'data-command)
	 ;; We have no data commands right now
	 (error "can't handle data commands yet"))
	((typep item 'code-command)
	 (let* ((operands (operands item))
		(candidates (candidates (mnemonic item) operands)))
	   (flet ((best-candidate (c1 c2)
		    (if (and (= (length operands) 1)
			     (typep (first operands) 'label))
			(if (> (instruction-size c1 operands)
			       (instruction-size c2 operands))
			    c1
			    c2)
			(if (< (instruction-size c1 operands)
			       (instruction-size c2 operands))
			    c1
			    c2))))
	     (encode-instruction (reduce #'best-candidate candidates)
				 operands))))
	(t
	 (error "Item of unknown type: ~s" item))))


(defun assemble (items)
  (let* ((preliminary-sizes (mapcar #'preliminary-size items))
	 (addresses (compute-preliminary-addresses items preliminary-sizes)))
    (let* ((*addresses* addresses)
	   (encodings (loop for item in items
			    for address = 0 then (+ address size)
			    for size in preliminary-sizes
			    collect
			    (let ((*instruction-pointer* (+ address size)))
			      (compute-encoding item)))))
      (coerce (reduce #'append encodings :from-end t)
	      '(simple-array (unsigned-byte 8) (*))))))
    
    
