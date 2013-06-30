(in-package #:sicl-arm-assembler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction descriptor.
;;;  
;;; An instruction descriptor is a string that describes the
;;; bit-fields of an instruction word.  An instruction descriptor
;;; contains exactly 65 characters.  The character \#| (vertical bar)
;;; is the first and the last character of the instruction descriptor,
;;; and it is also used to separate FIELDS in the descriptor.  Every
;;; vertical bar character in the descriptor occurs at an even
;;; position.  It follows, then, that a field always has an odd number
;;; of characters in it.  If L is the length of a field, then it
;;; represents a bit-field of width (L + 1)/2 in the instruction word.
;;;
;;; A field can be either a CONSTANT field or a VARIABLE field.  
;;;
;;; A constant field is a field that contains only characters #\0 and
;;; #\1 in odd positions, and #\Space character in all the even
;;; positions.  Such a field represents bits in the instruction word
;;; that must have exactly the value corresponding to the #\0 and #\1
;;; characters.
;;;
;;; A field is considered a variable field if it contains an
;;; alphabetic character.  A Lisp variable name (a symbol) is
;;; constructed from the field by stripping leading and trailing
;;; spaces and then taking the STRING-UPCASE of the remaining string,
;;; and finally INTERNing it in this package.


;;; From an instruction descriptor, compute a MASK in the form of a
;;; 32-bit unsigned integer that contains a 1 wherever the descriptor
;;; indicates a 0 or a 1 in the instruction word, and that contains a
;;; 0 in all other bit positions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-mask (descriptor)
    (loop with mask = 0
	  for index from 1 to 63 by 2
	  for weight downfrom 31
	  do (when (and (or (eql (char descriptor index) #\0)
			    (eql (char descriptor index) #\1))
			(or (eql (char descriptor (1- index)) #\Space)
			    (eql (char descriptor (1- index)) #\|))
			(or (eql (char descriptor (1+ index)) #\Space)
			    (eql (char descriptor (1+ index)) #\|)))
	       (setf (ldb (byte 1 weight) mask) 1))
	  finally (return mask))))
	
;;; From an instruction descriptor, compute a PATTERN in the form of a
;;; 32-bit unsigned integer that contains a 1 wherever the descriptor
;;; indicates a 1 in the instruction word, and that contains a 0 in
;;; all other bit positions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-pattern (descriptor)
    (loop with mask = 0
	  for index from 1 to 63 by 2
	  for weight downfrom 31
	  do (when (and (or (eql (char descriptor index) #\0)
			    (eql (char descriptor index) #\1))
			(or (eql (char descriptor (1- index)) #\Space)
			    (eql (char descriptor (1- index)) #\|))
			(or (eql (char descriptor (1+ index)) #\Space)
			    (eql (char descriptor (1+ index)) #\|)))
	       (when (eql (char descriptor index) #\1)
		 (setf (ldb (byte 1 weight) mask) 1)))
	  finally (return mask))))
	
;;; Return TRUE if and only if a field from an instruction descriptor
;;; is a VARIABLE field.  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun field-contains-variable-p (field)
    (find-if #'alpha-char-p field)))

;;; Given a VARIABLE field from an instruction descriptor, compute a
;;; Lisp variable name in the form of a symbol.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun variable-from-field (field)
    (intern (string-upcase (remove #\Space field))
	    (find-package '#:sicl-arm-assembler))))

;;; From an instruction descriptor, compute a list of LET BINDINGS.
;;; The variables of the bindings are the variables derived from
;;; variable fields, and the init-forms of the bindings are
;;; expressions of the form (LDB (BYTE x y) var), so that the body of
;;; the LET has the appropriate variables in scope, and those
;;; variables contain the values of the corresponding bit-fields in
;;; the instruction.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bindings (descriptor var)
    (let ((fields (split-sequence:split-sequence
		   #\| descriptor :remove-empty-subseqs t)))
      (assert (every (lambda (field) (oddp (length field))) fields))
      (assert (= (+ (reduce #'+ fields :key #'length)
		    (length fields))
		 64))
      (loop for field in fields
	    for left = 32 then (- left decr)
	    for decr = (/ (1+ (length field)) 2)
	    when (field-contains-variable-p field)
	      collect `(,(variable-from-field field)
			(ldb (byte ,decr ,(- left decr)) ,var))))))
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Disassembly according to assembly syntax.
;;;
;;; We take advantage of the regular instruction syntax to factor the
;;; code for instruction disassembly.  At the moment, we do it the
;;; stupid way, i.e., for each instance of an instruction list syntax
;;; that we see in the reference manual, we enter it into a list,
;;; associated with code that takes the instruction word and generates
;;; the arguments.


;;; Take a shift type between 0 and 3 and a numeric value between 0
;;; and 31.  If the shift type is 0 and the numeric is 0, then return
;;; the empty list.  If the shift type i 3 and the numeric value is 0,
;;; then return a list of a single element: RRX.  Otherwise return a
;;; list of two elements, the symbolic shift type and the shift amount
;;; corresponding to the numeric value for that particular type.
(defun optional-shift (shift-type value)
  (case shift-type
    (#b00
     (if (zerop value)
	 '()
	 `(LSL ,value)))
    (#b01
     (if (zerop value)
	 `(LSR 32)
	 `(LSR ,value)))
    (#b10
     (if (zerop value)
	 `(ASR 32)
	 `(ASR ,value)))
    (#b11
     (if (zerop value)
	 `(RRX)
	 `(ROR ,value)))))

;;; Take two register numbers.  If they are the same, return a list of
;;; a single element, the symbolic name of the register in question.
;;; If they are not the same, return a list of the symbolic names of
;;; both registers.
(defun optional-destination (Rd Rn)
  (if (= Rd Rn)
      `(,(reg-to-symbol Rn))
      `(,(reg-to-symbol Rd) ,(reg-to-symbol Rn))))

(defun arglist-immediate-type-1 (Rd Rn const)
  `(,@(optional-destination Rd Rn)
    ,const))

(defun arglist-immediate-type-2 (Rd const)
  `(,@(if (= Rd 13) '() `(,(reg-to-symbol Rd)))
    SP
    ,const))

(defun arglist-immediate-type-3 (Rd Rm const)
  `(,@(optional-destination Rd Rm)
    ,const))

(defun arglist-immediate-type-4 (cond imm12 imm4)
  (progn (if (/= cond #b1110)
	     (error "condition must be UNCONDITIONAL: ~a"
		    (cond-to-symbol cond))
	     `(,(+ (ash imm12 4) imm4)))))

(defun arglist-register-type-1 (Rd Rn Rm shift-type shift-value)
  `(,@(optional-destination Rd Rn)
    ,(reg-to-symbol Rm)
    ,@(optional-shift shift-type shift-value)))

(defun arglist-register-type-2 (Rd Rm shift-type shift-value)
  `(,@(if (= Rd 13) '() `(,(reg-to-symbol Rd)))
    SP
    ,(reg-to-symbol Rm)
    ,@(optional-shift shift-type shift-value)))

(defun arglist-register-type-3 (Rd Rn Rm)
  `(,@(optional-destination Rd Rn)
    ,(reg-to-symbol Rm)))

(defun arglist-register-type-4 (Rm)
  `(,(reg-to-symbol Rm)))

(defun arglist-register-shifted-register-type-1 (Rd Rn Rm shift-type Rs)
  `(,@(optional-destination Rd Rn)
    ,(reg-to-symbol Rm)
    ,(aref #(LSL LSR ASR ROR) shift-type)
    ,(reg-to-symbol Rs)))

(defun arglist-label-type-1 (Rd label-value)
  `(,(reg-to-symbol Rd)
    ,label-value))

(defun arglist-label-type-2 (label-value)
  `(,label-value))

(defun arglist-bitfield-type-1 (Rd msb lsb)
  `(,(reg-to-symbol Rd)
    ,lsb
    ,(1+ (- msb lsb))))

(defun arglist-bitfield-type-2 (Rd Rn msb lsb)
  `(,(reg-to-symbol Rd)
    ,(reg-to-symbol Rn)
    ,lsb
    ,(1+ (- msb lsb))))

(defun arglist-coproc-type-1 (coproc opc1 CRd CRn CRm opc2)
  `(,coproc
    ,opc1
    ,CRd
    ,CRn
    ,CRm
    ,@(if (zerop opc2) '() `(,opc2))))

(defun memaccess-type-1 (U Rt Rn offset)
  `(,(reg-to-symbol Rt)
    (,(reg-to-symbol Rn)
     ,@(if (and (= U 1) (= offset 0))
	   '()
	   (if (= U 1) `(,offset) `,(- offset))))))

(defun memaccess-type-2 (U Rt Rn offset)
  `(,(reg-to-symbol Rt)
    (,(reg-to-symbol Rn)
     ,@(if (= U 1) `(,offset) `,(- offset)))
    !))

(defun memaccess-type-3 (U Rt Rn offset)
  `(,(reg-to-symbol Rt)
    (,(reg-to-symbol Rn))
    ,@(if (= U 1) `(,offset) `,(- offset))))

(defun memaccess-type-4 (U Rt offset)
  `(,(reg-to-symbol Rt)
    (PC
     ,@(if (= U 1) `(,offset) `,(- offset)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *build-arguments*
    `((({<Rd>} <Rn> <const>)
       (arglist-immediate-type-1 Rd Rn imm12))
      (({<Rd>} SP <const>)
       (arglist-immediate-type-2 Rd imm12))
      (({<Rd>} <Rm> <imm>)
       (arglist-immediate-type-3 Rd Rm imm5))
      ((<imm16>)
       (arglist-immediate-type-4 cond imm12 imm4))
      (({<Rd>} <Rn> <Rm> {<shift>})
       (arglist-register-type-1 Rd Rn Rm typ imm5))
      (({<Rd>} SP <Rm> {<shift>})
       (arglist-register-type-2  Rd Rm typ imm5))
      (({<Rd>} <Rn> <Rm>)
       (arglist-register-type-3 Rd Rn Rm))
      ((<Rm>)
       (arglist-register-type-4 Rm))
      (({<Rd>} <Rn> <Rm> <type> <Rs>)
       (arglist-register-shifted-register-type-1 Rd Rn Rm typ Rs))
      ((<Rd> <label>)
       (arglist-label-type-1 Rd imm12))
      ((<label>)
       (arglist-label-type-2 imm24))
      ((<Rd> <lsb> <width>)
       (arglist-bitfield-type-1 Rd msb lsb))
      ((<Rd> <Rn> <lsb> <width>)
       (arglist-bitfield-type-2 Rd Rn msb lsb))
      ((<coproc> <opc1> <CRd> <CRn> CRm> {<opc2>})
       (arglist-coproc-type-1 coproc opc1 CRd CRn CRm opc2))
      ((<Rt> (<Rn>) +/-<imm>)
       (memaccess-type-1 U Rt Rn imm12))
      ((<Rt> (<Rn> {+/-<imm>}))
       (memaccess-type-2 U Rt Rn imm12))
      ((<Rt> (<Rn> +/-<imm>)!)
       (memaccess-type-3 U Rt Rn imm12))
      ((<Rt> (PC +/-<imm>))
       (memaccess-type-4 U Rt imm12)))))

;;; This function takes a symbol that is the first element of an
;;; assembly-syntax entry and generates the code for computing the
;;; symbol to appear first in the assembly version of the instruction.
(defun make-build-mnemonic (symbol)
  (let* ((name (symbol-name symbol))
	 (first-non-alpha (position-if-not #'alphanumericp name))
	 (base (subseq name 0 first-non-alpha))
	 (suffix (if (null first-non-alpha) "" (subseq name first-non-alpha))))
    (cond ((equal suffix "")
	   `',symbol)
	  ((equal suffix "{S}{<C>}")
	   `(build-name ,base
			(if (zerop S) "" "S")
			(cond-to-symbol cond)))
	  ((equal suffix "{S}")
	   `(build-name ,base
			(if (zerop S) "" "S")))
	  ((equal suffix "{<C>}")
	   `(build-name ,base
			(cond-to-symbol cond)))
	  ((equal suffix "<C>")
	   `(build-name ,base
			(if (= cond 14)
			    'AL
			    (cond-to-symbol cond))))
	  (t
	   (error "Unknown syntax: ~s" symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction info.

(defclass instruction ()
  ((%mask :initarg :mask :reader mask)
   (%pattern :initarg :pattern :reader pattern)
   (%syntax :initarg :syntax :reader syntax)
   (%additional-test :initarg :additional-test :reader additional-test)
   (%disassembler :initarg :disassembler :reader disassembler)))

(defmethod print-object ((object instruction) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (syntax object))))

(defparameter *instructions* '())
	  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-argument-disassembler (argument-list)
    (let ((entry (assoc argument-list *build-arguments* :test #'equal)))
      (when (null entry)
	(error "no argument encoder for argument list: ~s"
	       argument-list))
      (cadr entry))))

(defmacro define-instruction (descriptor additional-test syntax operation)
  (declare (ignore operation))
  (let ((mask (make-mask descriptor))
	(pattern (make-pattern descriptor))
	(bindings (bindings descriptor 'instruction))
	(mnemonic-code (make-build-mnemonic (car syntax)))
	(disassemby-code (make-argument-disassembler (cdr syntax))))
    `(push (make-instance 'instruction
	     :mask ,mask
	     :pattern ,pattern
	     :additional-test
	     (lambda (instruction)
	       (declare (ignorable instruction))
	       (let ,bindings
		 (declare (ignorable ,@(mapcar #'car bindings)))
		 ,additional-test))
	     :syntax ',syntax
	     :disassembler
	     (lambda (instruction)
	       (let ,bindings
		 (cons ,mnemonic-code
		       ,disassemby-code))))
	   *instructions*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the disassembly of an instruction word.

(defun cond-to-symbol (cond)
  (check-type cond (integer 0 14))
  (aref #(EQ NE CS CC MI PL VS VC HI LS GE LT GT LE ||)
	cond))

(defun reg-to-symbol (reg)
  (check-type reg (integer 0 15))
  (aref #(R0 R1 R2  R3  R4  R5 R6 R7 R8 R9 R10 R11 R12 SP LR PC)
	reg))

(defun reg-list-to-symbol (reg-list)
  (let ((result ""))
    (loop for i from 0 below 16
	  do (unless (zerop (ldb (byte 1 i) reg-list))
	       (setf result
		     (concatenate 'string result (reg-to-symbol i) " "))))
    result))

(defun shift-type-to-symbol (shift-type)
  (check-type shift-type (integer 0 3))
  (aref #(LSL LSR ASR ROR) shift-type))

(defun find-entry (instruction)
  (find-if (lambda (entry)
	     (and (= (logand instruction (mask entry))
		     (pattern entry))
		  (funcall (additional-test entry) instruction)))
	   *instructions*))

(defun handle-instruction (instruction)
  (let ((entry (find-entry instruction)))
    (if (null entry)
	(error "unknown instruction ~a" instruction)
	(funcall (syntax entry) instruction))))

(defun build-name (&rest string-designators)
  (intern (apply #'concatenate 'string
		 (mapcar #'string string-designators))
	  (find-package '#:sicl-arm-assembler)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADC (immediate)
;;;
;;; Add with Carry (immediate) adds an immediate value and the Carry
;;; flag value to a register value, and writes the result to the
;;; destination register.  If S is present, updates the condition
;;; flag.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 1|S|   Rn  |   Rd  |       imm12           |"
    (not (and (equal Rd #*1111) (equal S #*1)))
  (ADC{S}{<c>} {<Rd>} <Rn> <const>)
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1))
	(imm32 (arm-expand-imm im12)))
    (when (condition-passed)
      (multiple-value-bind (result carry overflow)
	  (add-with-carry (reg n) imm32 (APSR +C+))
	(if (= d 15)              ; can only occur for ARM encoding
	    (alu-write-pc result) ; setflags is always false here
	    (progn (setf (reg d) result)
		   (setf (APSR +N+) (logbitp 31 result))
		   (setf (APSR +Z+) (if (zerop result) 1 0)
		   (setf (APSR +C+) carry)
		   (setf (APSR +V+) overflow))))))))
	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADC (register) 
;;;
;;; Add with Carry (register) adds a register value, the Carry flag
;;; value, and an optionally-shifted register value, and writes the
;;; result to the destination register. It can optionally update the
;;; condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 1 0 1|S|   Rn  |   Rd  |   imm5  |typ|0|  Rm   |"
    (not (and (equal Rd #*1111) (equal S #*1)))
  (ADC{S}{<c>} {<Rd>} <Rn> <Rm> {<shift>})
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when (condition-passed)
	(let ((shifted (shift (reg m) shift-t shift-n (APSR +C+))))
	  (if (= d 15)              ; can only occur for ARM encoding
	      (alu-write-pc result) ; setflags is always false here
	      (progn (setf (reg d) result)
		   (setf (APSR +N+) (logbitp 31 result))
		   (setf (APSR +Z+) (if (zerop result) 1 0)
		   (setf (APSR +C+) carry)
		   (setf (APSR +V+) overflow)))))))))
		     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADC (register-shifted register)
;;;
;;; Add with Carry (register-shifted register) adds a register value,
;;; the Carry flag value, and a register-shifted register value. It
;;; writes the result to the destination register, and can optionally
;;; update the condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 1 0 1|S|   Rn  |   Rd  |   Rs  |0|typ|1|  Rm   |"
    t
  (ADC{S}{<c>} {<Rd>} <Rn> <Rm> <type> <Rs>)
  (let ((d (u-int Rd)
	(n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(setflags (equal S #*1))
	(shift-t (decode-reg-shift typ))))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredicatable)
    (when condition-passed
      (let* ((shift-n (ldb (byte 8 0) (reg s)))
	     (shifted (shift (reg m) shift-t shift-n (APSR +C))))
	(multiple-value-bind (result carry overflow)
	    (add-with-carry (reg n) shifted (APSR +C+))
	  (setf (reg d) result)
	  (when setflags
		   (setf (APSR +N+) (logbitp 31 result))
		   (setf (APSR +Z+) (if (zerop result) 1 0)
		   (setf (APSR +C+) carry)
		   (setf (APSR +V+) overflow))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADD (immediate)
;;; 
;;; This instruction adds an immediate value to a register value, and
;;; writes the result to the destination register. It can optionally
;;; update the condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 0|S|   Rn  |   Rd  |       imm12           |"
    (not (or (and (equal Rn #*1111) (equal S #*0))
	     (equal Rn #*1101)
	     (and (equal Rd #*1111) (equal S #*1))))
  (ADD{S}{<c>} {<Rd>} <Rn> <const>)
  ())

;;; Instruction: ADD (register)
;;; 
;;; This instruction adds a register value and an optionally-shifted
;;; register value, and writes the result to the destination
;;; register. It can optionally update the condition flags based on
;;; the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 1 0 0|S|   Rn  |   Rd  |   imm5  |typ|0|  Rm   |"
    (not (or (and (equal Rd #*1111) (equal S #*1))
	     (equal Rn #*1101)))
  (ADD{S}{<c>} {<Rd>} <Rn> <Rm> {<shift>})
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADD (register-shifted register)
;;; 
;;; Add (register-shifted register) adds a register value and a
;;; register-shifted register value. It writes the result to the
;;; destination register, and can optionally update the condition
;;; flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 1 0 0|S|   Rn  |   Rd  |   Rs  |0|typ|1|  Rm   |"
    t
  (ADD{S}{<c>} {<Rd>} <Rn> <Rm> <type> <Rs>)
  ())
  
;;; Instruction: ADD (SP plus immediate)
;;; 
;;; This instruction adds an immediate value to the SP value, and
;;; writes the result to the destination register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 0|S|1 1 0 1|   Rd  |       imm12           |"
    (not (and (equal Rd #*1111) (equal S #*1)))
  (ADD{S}{<c>} {<Rd>} SP <const>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADD (SP plus register)
;;; 
;;; This instruction adds an optionally-shifted register value to the
;;; SP value, and writes the result to the destination register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 1 0 0|S|1 1 0 1|   Rd  |   imm5  |typ|0|  Rm   |"
    (not (and (equal Rd #*1111) (equal S #*1)))
  (ADD{S}{<c>} {<Rd>} SP <Rm> {<shift>})
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADR
;;; 
;;; This instruction adds an immediate value to the PC value to form a
;;; PC-relative address, and writes the result to the destination
;;; register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 0|0|1 1 1 1|   Rd  |       imm12           |"
  t
  (ADR{<c>} <Rd> <label>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: AND (immediate)
;;; 
;;; This instruction performs a bitwise AND of a register value and an
;;; immediate value, and writes the result to the destination
;;; register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 0 0 0|S|   Rn  |   Rd  |       imm12           |"
  (not (and (equal rd #*1111) (equal S #*1)))
  (AND{S}{<c>} {<Rd>} <Rn> <const>)
  ())
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: AND (register)
;;; 
;;; This instruction performs a bitwise AND of a register value and an
;;; optionally-shifted register value, and writes the result to the
;;; destination register. It can optionally update the condition flags
;;; based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 0 0 0|S|   Rn  |   Rd  |   imm5  |typ|0|  Rm   |"
    (not (and (equal Rd #*1111) (equal S #*1)))
  (AND{S}{<c>} {<Rd>} <Rn> <Rm> {<shift>})
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: AND (register-shifted register)
;;; 
;;; This instruction performs a bitwise AND of a register value and a
;;; register-shifted register value. It writes the result to the
;;; destination register, and can optionally update the condition
;;; flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 0 0 0|S|   Rn  |   Rd  |   Rs  |0|typ|1|  Rm   |"
    t
  (AND{S}{<c>} {<Rd>} <Rn> <Rm> <type> <Rs>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ASR (immediate)
;;; 
;;; Arithmetic Shift Right (immediate) shifts a register value right by an
;;; immediate number of bits, shifting in copies of its sign bit, and
;;; writes the result to the destination register. It can optionally
;;; update the condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|1 1 0 1|S| (0000)|   Rd  |   imm5  |1 0 0|  Rm   |"
    (not (and (equal Rd #*1111)
	      (equal S #*1)))
  (ASR{S}{<c>} {<Rd>} <Rm> <imm>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ASR (register)
;;; 
;;; Arithmetic Shift Right (register) shifts a register value right by
;;; a variable number of bits, shifting in copies of its sign bit, and
;;; writes the result to the destination register. The variable number
;;; of bits is read from the bottom byte of a register. It can
;;; optionally update the condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|1 1 0 1|S| (0000)|   Rd  |   Rm  |0 1 0 0|  Rn   |"
    t
  (ASR{S}{<c>} {<Rd>} <Rn> <Rm>)
  ())
    
;;; Instruction: B
;;; 
;;; Branch causes a branch to a target address.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |1 0 1 0|                   imm24                       |"
    t
  (B{<c>} <label>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BFC
;;;
;;; Bit Field Clear clears any number of adjacent bits at any position
;;; in a register, without affecting the other bits in the register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 1 1 1 1 0|   msb   |  Rd   |   lsb   |0 0 1|1 1 1 1|"
    t
  (BFC{<c>} <Rd> <lsb> <width>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BFI
;;;
;;; Bit Field Insert copies any number of low order bits from a
;;; register into the same number of adjacent bits at any position in
;;; the destination register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 1 1 1 1 0|   msb   |  Rd   |   lsb   |0 0 1|  Rn   |"
    (not (equal Rd #*1111))
  (BFI{<c>} <Rd> <Rn> <lsb> <width>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BIC (immediate)
;;;
;;; Bitwise Bit Clear (immediate) performs a bitwise AND of a register
;;; value and the complement of an immediate value, and writes the
;;; result to the destination register. It can optionally update the
;;; condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|1 1 1 0|S|  Rn   |  Rd   |        imm12          |"
    (not (and (equal Rd #*1111)
	      (equal S #*1)))
  (BIC{S}{<c>} {<Rd>} <Rn> <const>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BKPT
;;;
;;; Breakpoint causes a software breakpoint to occur.
;;; Breakpoint is always unconditional, even when inside an IT block.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0 0 1 0 0 1 0|         imm12         |0 1 1 1| imm4  |"
    t
  (BKPT <imm16>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BL (immediate)
;;;
;;; Branch with Link calls a subroutine at a PC-relative address.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |1 0 1 1|                   imm24                       |"
    t
  (BL<c> <label>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BLX (register)
;;;
;;; Branch with Link and Exchange (register) calls a subroutine at an
;;; address and instruction set specified by a register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0 0 1 0 0 1 0|1 1 1 1|1 1 1 1|1 1 1 1|0 0 1 1|  Rm   |"
    t
  (BLX{<c>} <Rm>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BX
;;;
;;; Branch and Exchange causes a branch to an address and instruction
;;; set specified by a register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0 0 1 0 0 1 0|1 1 1 1|1 1 1 1|1 1 1 1|0 0 0 1|  Rm   |"
    t
  (BX{<c>} <Rm>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BXJ
;;;
;;; Branch and Exchange Jazelle attempts to change to Jazelle
;;; state. If the attempt fails, it branches to an address and
;;; instruction set specified by a register as though it were a BX
;;; instruction.  In an implementation that includes the
;;; Virtualization Extensions, if HSTR.TJDBX is set to 1, execution of
;;; a BXJ instruction in a Non-secure mode other than Hyp mode
;;; generates a Hyp Trap exception.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0 0 1 0 0 1 0|1 1 1 1|1 1 1 1|1 1 1 1|0 0 1 0|  Rm   |"
    t
  (BXJ{<c>} <Rm>)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CDP
;;; 
;;; Coprocessor Data Processing tells a coprocessor to perform an
;;; operation that is independent of ARM core registers and memory. If
;;; no coprocessor can execute the instruction, an Undefined
;;; Instruction exception is generated.  This is a generic coprocessor
;;; instruction. Some of the fields have no functionality defined by
;;; the architecture and are free for use by the coprocessor
;;; instruction set designer. These are the opc1, opc2, CRd, CRn, and
;;; CRm fields.  However, coprocessors CP8-CP15 are reserved for use
;;; by ARM, and this manual defines the valid CDP and CDP2
;;; instructions when coproc is in the range p8-p15
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |1 1 1 0| opc1  |  CRn  |  CRd  |coproc |opc2 |0|  CRm  |"
    (not (and (/= coproc #x1010) (/= coproc #x1011)))
  (CDP{<c>} <coproc> <opc1> <CRd> <CRn> CRm> {<opc2>})
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CDP2
;;;
;;; Coprocessor Data Processing tells a coprocessor to perform an
;;; operation that is independent of ARM core registers and memory. If
;;; no coprocessor can execute the instruction, an Undefined
;;; Instruction exception is generated.  This is a generic coprocessor
;;; instruction. Some of the fields have no functionality defined by
;;; the architecture and are free for use by the coprocessor
;;; instruction set designer. These are the opc1, opc2, CRd, CRn, and
;;; CRm fields.  However, coprocessors CP8-CP15 are reserved for use
;;; by ARM, and this manual defines the valid CDP and CDP2
;;; instructions when coproc is in the range p8-p15
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "|1 1 1 1|1 1 1 0| opc1  |  CRn  |  CRd  |coproc |opc2 |0|  CRm  |"
    (not (and (/= coproc #x1010) (/= coproc #x1011)))
  (CDP2 <coproc> <opc1> <CRd> <CRn> CRm> {<opc2>})
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: LDR (immediate)
;;;
;;; Load Register (immediate) calculates an address from a base
;;; register value and an immediate offset, loads a word from memory,
;;; and writes it to a register. It can use offset, post-indexed, or
;;; pre-indexed addressing.
;;;
;;; To get the assembly syntax closer to the ARM standard, we encode
;;; this instruction as three different instance: 
;;; P = 0 & W = 0; P = 1 & W = 0; P = 1 & W = 1

;;; P = 0 & W = 0
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|0|U|0|0|1|  Rn   |  Rt   |         imm12         |"
    (not (or (equal Rn #*1111)
	     (and (equal Rn #*1101) (= U 1) (= imm12 4))))
  (LDR{<c>} <Rt> (<Rn>) +/-<imm>)
  ())

;;; P = 1 & W = 0
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|0|1|  Rn   |  Rt   |         imm12         |"
    (not (or (equal Rn #*1111)
	     (and (equal Rn #*1101) (= U 1) (= imm12 4))))
  (LDR{<c>} <Rt> (<Rn> {+/-<imm>}))
  ())

;;; P = 1 & W = 1
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|1|1|  Rn   |  Rt   |         imm12         |"
    (not (equal Rn #*1111))
  (LDR{<c>} <Rt> (<Rn> +/-<imm>)!)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: LDR (literal)
;;;
;;; Load Register (literal) calculates an address from the PC value
;;; and an immediate offset, loads a word from memory, and writes it
;;; to a register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|0|1|1 1 1 1|  Rt   |         imm12         |"
    t
    (LDR{<c>} <Rt> (PC +/-<imm>))
  ())
