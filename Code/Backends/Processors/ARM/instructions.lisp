(in-package #:sicl-arm-assembler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction descriptor.
;;;  
;;; An instruction descriptor is a string that describes the
;;; bit-fields of an instruction word.  An instruction descriptor
;;; contains exactly 65 characters.  The character #\| (vertical bar)
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
    (loop with mask = (make-array 32 :element-type 'bit :initial-element 0)
	  for index from 1 to 63 by 2
	  for mask-index from 0
	  do (when (and (or (eql (char descriptor index) #\0)
			    (eql (char descriptor index) #\1))
			(or (eql (char descriptor (1- index)) #\Space)
			    (eql (char descriptor (1- index)) #\|))
			(or (eql (char descriptor (1+ index)) #\Space)
			    (eql (char descriptor (1+ index)) #\|)))
	       (setf (sbit mask mask-index) 1))
	  finally (return mask))))
	
;;; From an instruction descriptor, compute a PATTERN in the form of a
;;; 32-bit unsigned integer that contains a 1 wherever the descriptor
;;; indicates a 1 in the instruction word, and that contains a 0 in
;;; all other bit positions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-pattern (descriptor)
    (loop with pattern = (make-array 32 :element-type 'bit :initial-element 0)
	  for index from 1 to 63 by 2
	  for pattern-index from 0
	  do (when (and (or (eql (char descriptor index) #\0)
			    (eql (char descriptor index) #\1))
			(or (eql (char descriptor (1- index)) #\Space)
			    (eql (char descriptor (1- index)) #\|))
			(or (eql (char descriptor (1+ index)) #\Space)
			    (eql (char descriptor (1+ index)) #\|)))
	       (when (eql (char descriptor index) #\1)
		 (setf (sbit pattern pattern-index) 1)))
	  finally (return pattern))))
	
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
;;; expressions of the form (SUBSEQ var start end) , so that the body
;;; of the LET has the appropriate variables in scope, and those
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
	    for start = 0 then (+ start decr)
	    for decr = (/ (1+ (length field)) 2)
	    when (field-contains-variable-p field)
	      collect `(,(variable-from-field field)
			(subseq ,var ,start ,(+ start decr)))))))
						

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definitions of functions and macros for pseudocode support.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp equivalents of psudocode as defined in the manual.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations on bit vectors.
;;;
;;; Secton P.5.3, page 2651

(defun top-bit (x)
  (assert (typep x 'bit-vector))
  (subseq x 0 1))

;;; We can't use :: as a symbol name so we use CAT instead.
(defun cat (x y)
  (assert (typep x 'bit-vector))
  (assert (typep y 'bit-vector))
  (concatenate 'bit-vector x y))

(defun replicate (x n)
  (assert (typep x 'bit-vector))
  (assert (typep n '(integer (0))))
  (loop with result = #*
	repeat n
	do (setf result (cat result x))))

(defun zeros (n)
  (assert (typep n '(integer (0))))
  (make-array n :element-type 'bit :initial-element 0))

(defun ones (n)
  (assert (typep n '(integer (0))))
  (make-array n :element-type 'bit :initial-element 1))

(defun is-zero (x)
  (assert (typep x 'bit-vector))
  (zerop (count 1 x)))

(defun is-ones (x)
  (assert (typep x 'bit-vector))
  (zerop (count 0 x)))

(defun is-zero-bit (x)
  (assert (typep x 'bit-vector))
  (if (is-zero x) #*1 #*0))

(defun is-ones-bit (x)
  (assert (typep x 'bit-vector))
  (if (is-ones x) #*1 #*0))

(defun lowest-bit-set (x)
  (assert (typep x 'bit-vector))
  (let ((pos (position 1 x :from-end t))
	(length (length x)))
    (if (null pos)
	length
	(- length pos 1))))

(defun highest-bit-set (x)
  (assert (typep x 'bit-vector))
  (let ((pos (position 1 x))
	(length (length x)))
    (if (null pos)
	-1
	(- length pos 1))))

(defun count-leading-zero-bits (x)
  (assert (typep x 'bit-vector))
  (let ((pos (position 1 x))
	(length (length x)))
    (if (null pos)
	length
	pos)))

(defun count-leading-sign-bits (x)
  (assert (typep x 'bit-vector))
  (let ((pos (position (- 1 (sbit x 0)) x))
	(length (length x)))
    (if (null pos)
	length
	pos)))

;;; ZERO-EXTEND takes a bitvector and a size and extends the bitvector
;;; with 0s to the number of bits given by size.
(defun zero-extend (bitvector size)
  (assert (> size (length bitvector)))
  (let ((result (make-array size :element-type 'bit :initial-element 0)))
    (setf (subseq result (- size (length bitvector))) bitvector)
    result))

(defun sign-extend (bitvector size)
  (let ((result (make-array size :element-type 'bit
				 :initial-element (sbit bitvector 0))))
    (setf (subseq result (- size (length bitvector))) bitvector)
    result))

(defun u-int (bitvector)
  (loop with result = 0
	for bit across bitvector
	do (setf result (+ (ash result 1) bit))
	finally (return result)))

(defun s-int (bitvector)
  (let ((u-int (u-int bitvector)))
    (if (= (sbit bitvector 0) 1)
	(- u-int (expt 2 (length bitvector)))
	u-int)))

(defun int (bitvector unsigned-p)
  (if unsigned-p
      (u-int bitvector)
      (s-int bitvector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Arithmetic
;;;
;;; Section P.5.4, page 2654

(defun integer-to-bit-vector (n width)
  (let ((result (make-array width :element-type 'bit :initial-element 0)))
    (loop for i downfrom (1- width) to 0
	  for weight from 0
	  do (setf (sbit result i) (ldb (byte 1 weight) n)))
    result))

(defgeneric <> (x high low))

(defmethod <> ((x bit-vector) high low)
  (let ((length (length x)))
    (subseq x (- length high 1) (- length low))))

(defmethod <> ((x integer) high low)
  (let ((width (1+ (- high low))))
    (integer-to-bit-vector (ldb (byte width low) x) width))) 

(defgeneric (setf <>) (new-interval x high low))

(defmethod (setf <>) (new-interval (x bit-vector) high low)
  (let ((length (length x)))
    (setf (subseq x (- length high 1) (- length low))
	  new-interval)))

(defgeneric binary-add (x y))

(defmethod binary-add ((x integer) (y integer))
  (+ x y))

(defmethod binary-add ((x bit-vector) (y bit-vector))
  (assert (= (length x) (length y)))
  (<> (add (u-int x) (u-int y)) (1- (length x)) 0))

(defmethod binary-add ((x bit-vector) (y integer))
  (binary-add x (<> y (1- (length x)) 0)))

(defmethod binary-add ((x integer) (y bit-vector))
  (binary-add (<> x (1- (length y)) 0) y))

(defun add (term &rest more-terms)
  (if (null more-terms)
      term
      (apply #'add (binary-add term (car more-terms)) (cdr more-terms))))

(defgeneric binary-sub (x y))

(defmethod binary-sub ((x integer) (y integer))
  (- x y))

(defmethod binary-sub ((x bit-vector) (y bit-vector))
  (assert (= (length x) (length y)))
  (<> (sub (u-int x) (u-int y)) (1- (length x)) 0))

(defmethod binary-sub ((x bit-vector) (y integer))
  (binary-sub x (<> y (1- (length x)) 0)))

(defmethod binary-sub ((x integer) (y bit-vector))
  (binary-sub (<> x (1- (length y)) 0) y))

(defun sub (term &rest more-terms)
  (cond ((null more-terms)
	 (binary-sub 0 term))
	((null (cdr more-terms))
	 (binary-sub term (car more-terms)))
	(t
	 (apply #'sub (binary-sub term (car more-terms)) (cdr more-terms)))))

(defgeneric align (x y))

(defmethod align ((x integer) (y integer))
  (assert (not (zerop y)))
  (* y (floor x y)))

(defmethod align ((x bit-vector) (y integer))
  (assert (not (zerop y)))
  (<> (align (u-int x) y) (1- (length x)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Shift and rotate operations.
;;;
;;; Section A2.2.1, page 41

(defun lsl-c (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift '(integer (0))))
  (let* ((N (length x))
	 (extended-x (cat x (zeros shift)))
	 (result (<> extended-x (1- N) 0))
	 (carry-out (<> extended-x N N)))
    (values result carry-out)))

(defun lsl (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift '(integer 0)))
  (if (zerop shift)
      x
      (nth-value 0 (lsl-c x shift))))

(defun lsr-c (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift '(integer (0))))
  (let* ((N (length x))
	 (extended-x (zero-extend x (+ N shift)))
	 (result (<> extended-x (1- (+ N shift)) shift))
	 (carry-out (<> extended-x (1- shift) (1- shift))))
    (values result carry-out)))

(defun lsr (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift '(integer 0)))
  (if (zerop shift)
      x
      (nth-value 0 (lsr-c x shift))))

(defun asr-c (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift '(integer (0))))
  (let* ((N (length x))
	 (extended-x (sign-extend x (+ N shift)))
	 (result (<> extended-x (1- (+ N shift)) shift))
	 (carry-out (<> extended-x (1- shift) (1- shift))))
    (values result carry-out)))

(defun asr (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift '(integer 0)))
  (if (zerop shift)
      x
      (nth-value 0 (asr-c x shift))))
  
(defun ror-c (x shift)
  (assert (typep x 'bit-vector))
  (assert (and (typep shift 'integer) (not (zerop shift)))) 
  (let* ((N (length x))
	 (m (mod shift N))
	 (result (bit-ior (lsr x m) (lsl x (- N m))))
	 (carry-out (<> result (1- N) (1- N))))
    (values result carry-out)))

(defun ror (x shift)
  (assert (typep x 'bit-vector))
  (assert (typep shift 'integer))
  (if (zerop shift)
      x
      (nth-value 0 (ror-c x shift))))

(defun rrx-c (x carry-in)
  (assert (typep x 'bit-vector))
  (assert (typep carry-in 'bit-vector))
  (let* ((N (length x))
	 (result (cat carry-in (<> x (1- N) 1)))
	 (carry-out (<> x 0 0)))
    (values result carry-out)))

(defun rrx(x carry-in)
  (assert (typep x 'bit-vector))
  (assert (typep carry-in 'bit-vector))
  (nth-value 0 (rrx-c x carry-in)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addition and subtraction.
;;;
;;; Section A2.2.1, page 44

(defun add-with-carry (x y carry-in)
  (assert (and (typep x 'bit-vector)
	       (typep y 'bit-vector)
	       (typep carry-in '(bit-vector 1))
	       (= (length x) (length y))))
  (let* ((N (length x))
	 (unsigned-sum (add (u-int x) (u-int y) (u-int carry-in)))
	 (signed-sum (add (s-int x) (s-int y) (u-int carry-in)))
	 (result (<> unsigned-sum (1- N) 0))
	 (carry-out (if (= (u-int result) unsigned-sum) #*0 #*1))
	 (overflow (if (= (s-int result) signed-sum) #*0 #*1)))
    (values result carry-out overflow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saturation.
;;;
;;; Section A2.2.1, page 44.

;;; FIXME: fill in when needed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pseudocode detail of operatons on ARM core registers.
;;;
;;; Section A2.3.2, page 47.

;;; The current instance of the machine.
(defvar *machine*)

(defun reg (register-number)
  (assert (<= 0 register-number 15))
  (aref (registers *machine*) register-number))
	  
(defun (setf reg) (new-contents register-number)
  (assert (<= 0 register-number 15))
  (assert (typep new-contents '(bit-vector 32)))
  (setf (aref (registers *machine*) register-number)
	new-contents))
  
(defun pc-store-value ()
  (aref (registers *machine*) 15))

;;; Section B1.3.2, page 1147.
(defun branch-to (address)
  (assert (typep address '(bit-vector 32)))
  (setf (aref (registers *machine*) 15) address))

;;; We skip the tests for instruction set and architecture version. 
(defun branch-write-pc (address)
  (assert (typep address '(bit-vector 32)))
  (assert (equal (<> address 1 0) #*00))
  (branch-to address))

;;; Section A2.3.2, page 48.
;;; We skip the test for architecture version, etc.
(defun alu-write-pc (address)
  (assert (typep address '(bit-vector 32)))
  (branch-write-pc address))

;;; Section A2.3.2, page 48.
;;; We skip the test for architecture version, etc.
(defun load-write-pc (address)
  (branch-to address))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Operations of modified immediate constants
;;;
;;; Section A5.2.4, page 201.

(defun arm-expand-imm (imm12)
  (assert (typep imm12 '(bit-vector 12)))
  (nth-value 0 (arm-expand-imm-c imm12 APSR.C)))

(defun arm-expand-imm-c (imm12 carry-in)
  (assert (typep imm12 '(bit-vector 12)))
  (assert (typep carry-in '(bit-vector 1)))
  (let ((unrotated-value (zero-extend (<> imm12 7 0) 32)))
    (shift-c unrotated-value
	     :sr-type-ror
	     (* 2 (u-int (<> imm12 11 8)))
	     carry-in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pseudocode details of instruction-specified shifts and rotates
;;;
;;; Section A8.4.3, page 292.

(defun decode-imm-shift (type imm5)
  (assert (typep type '(bit-vector 2)))
  (assert (typep imm5 '(bit-vector 5)))
  (cond ((equal type #*00)
	 (values :sr-type-lsl (u-int imm5)))
	((equal type #*01)
	 (values :sr-type-lsr (if (equal imm5 #*0000) 32 (u-int imm5))))
	((equal type #*10)
	 (values :sr-type-asr (if (equal imm5 #*0000) 32 (u-int imm5))))
	((equal type #*11)
	 (if (equal imm5 #*0000)
	     (values :sr-type-rrx 1)
	     (values :sr-type-ror (u-int imm5))))))

(defun decode-reg-shift (type)
  (assert (typep type '(bit-vector 2)))
  (cond ((equal type #*00) :sr-type-lsl)
	((equal type #*01) :sr-type-lsr)
	((equal type #*10) :sr-type-asr)
	((equal type #*11) :sr-type-ror)))

(deftype sr-type ()
  `(member :sr-type-lsl :sr-type-lsr :sr-type-asr :sr-type-ror :sr-type-rrx))

(defun shift (value type amount carry-in)
  (assert (typep value 'bit-vector))
  (assert (typep type 'sr-type))
  (assert (typep carry-in '(bit-vector 1)))
  (nth-value 0 (shift-c value type amount carry-in)))

(defun shift-c (value type amount carry-in)
  (assert (typep value 'bit-vector))
  (assert (typep type 'sr-type))
  (assert (typep carry-in '(bit-vector 1)))
  (if (zerop amount)
      (values value carry-in)
      (ecase type
	(:sr-type-lsl (lsl-c value amount))
	(:sr-type-lsr (lsr-c value amount))
	(:sr-type-asl (asr-c value amount))
	(:sr-type-ror (ror-c value amount))
	(:sr-type-rrx (rrx-c value amount)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions and macros to access the machine.

(define-symbol-macro APSR.N
    (subseq (condition-codes *machine*) 0 1))
(define-symbol-macro APSR.Z
    (subseq (condition-codes *machine*) 1 2))
(define-symbol-macro APSR.C
    (subseq (condition-codes *machine*) 2 3))
(define-symbol-macro APSR.V
    (subseq (condition-codes *machine*) 3 4))
(define-symbol-macro APSR.Q
    (subseq (condition-codes *machine*) 4 5))

(define-symbol-macro condition-passed
  (cond ((equal cond #*0000)
	 (equal APSR.Z #*1))
	((equal cond #*0001)
	 (equal APSR.Z #*0))
	((equal cond #*0010)
	 (equal APSR.C #*1))
	((equal cond #*0011)
	 (equal APSR.C #*0))
	((equal cond #*0100)
	 (equal APSR.N #*1))
	((equal cond #*0101)
	 (equal APSR.N #*0))
	((equal cond #*0110)
	 (equal APSR.V #*1))
	((equal cond #*0111)
	 (equal APSR.V #*0))
	((equal cond #*1000)
	 (and (equal APSR.C #*1) (equal APSR.Z #*0)))
	((equal cond #*1001)
	 (or (equal APSR.C #*0) (equal APSR.Z #*1)))
	((equal cond #*1010)
	 (equal APSR.N APSR.V))
	((equal cond #*1011)
	 (not (equal APSR.N APSR.V)))
	((equal cond #*1100)
	 (and (equal APSR.Z #*0) (equal APSR.N APSR.V)))
	((equal cond #*1101)
	 (or (equal APSR.Z #*1) (not (equal APSR.N APSR.V))))
	((equal cond #*1110)
	 t)))

(define-symbol-macro unpredictable
    (warn "unpredictable behavior"))

(defun mem-u (address number-of-bytes)
  (integer-to-bit-vector
   (load-memory-unaligned (memory *machine*) address number-of-bytes)
   (* number-of-bytes 8)))

(defun (setf mem-u) (value address number-of-bytes)
  (store-memory-unaligned
   (memory *machine*) address (u-int value) number-of-bytes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction info.

(defclass instruction ()
  ((%mask :initarg :mask :reader mask)
   (%pattern :initarg :pattern :reader pattern)
   (%syntax :initarg :syntax :reader syntax)
   (%additional-test :initarg :additional-test :reader additional-test)
   (%disassembler :initarg :disassembler :reader disassembler)
   (%operation :initarg :operation :reader operation)))

(defparameter *instructions* '())
	  
(defmacro define-instruction (descriptor additional-test disassembler operation)
  (let ((mask (make-mask descriptor))
	(pattern (make-pattern descriptor))
	(bindings (bindings descriptor 'instruction)))
    `(push (make-instance 'instruction
	     :mask ,mask
	     :pattern ,pattern
	     :additional-test
	     (lambda (instruction)
	       (declare (ignorable instruction))
	       (let ,bindings
		 (declare (ignorable ,@(mapcar #'car bindings)))
		 ,additional-test))
	     :disassembler
	     (lambda (instruction stream)
	       (let ,bindings
		 ,disassembler))
	     :operation
	     (lambda (instruction)
	       (let ,bindings
		 ,operation)))
	   *instructions*)))

;;; Given an instruction in the form of a bitvector of length 32, find
;;; an instruction entry that matches that instruction.  Position 0 of
;;; the bitvector corresponds to bit 31 in the corresponding unsigned
;;; 32-bit integer that make up the instruction in memory.
(defun find-entry (instruction)
  (find-if (lambda (entry)
	     (and (equal (bit-and instruction (mask entry))
		     (pattern entry))
		  (funcall (additional-test entry) instruction)))
	   *instructions*))

(defun execute-instruction (instruction)
  (let ((entry (find-entry instruction)))
    (when (null entry)
      (error "no such instruction ~s" instruction))
    (funcall (operation entry) instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for disassembly.

(defun condition-to-string (condition)
  (cond ((equal condition #*0000) "EQ")
	((equal condition #*0001) "NE")
	((equal condition #*0010) "CS")
	((equal condition #*0011) "CC")
	((equal condition #*0100) "MI")
	((equal condition #*0101) "PL")
	((equal condition #*0110) "VS")
	((equal condition #*0111) "VC")
	((equal condition #*1000) "HI")
	((equal condition #*1001) "LS")
	((equal condition #*1010) "GE")
	((equal condition #*1011) "LT")
	((equal condition #*1100) "GT")
	((equal condition #*1101) "LE")
	((equal condition #*1110) "")))

(defun register-to-string (register)
  (cond ((equal register #*0000) "R0")
	((equal register #*0001) "R1")
	((equal register #*0010) "R2")
	((equal register #*0011) "R3")
	((equal register #*0100) "R4")
	((equal register #*0101) "R5")
	((equal register #*0110) "R6")
	((equal register #*0111) "R7")
	((equal register #*1000) "R8")
	((equal register #*1001) "R9")
	((equal register #*1010) "R10")
	((equal register #*1011) "R11")
	((equal register #*1100) "R12")
	((equal register #*1101) "SP")
	((equal register #*1110) "LR")
	((equal register #*1111) "PC")))

(defun optional-register-to-string (Rd Rn)
  (if (equal Rd Rn)
      ""
      (concatenate 'string (register-to-string Rd) ", ")))

(defun optional-shift-of-register (type imm5)
  (cond ((equal type #*00)
	 (if (equal imm5 #*00000)
	     ""
	     (format nil ", LSL #~a" (u-int imm5))))
	((equal type #*01)
	 (format nil ", LSR #~d"
		 (if (equal imm5 #*00000) 32 (u-int imm5))))
	((equal type #*10)
	 (format nil ", ASR #~d"
		 (if (equal imm5 #*00000) 32 (u-int imm5))))
	((equal type #*11)
	 (format nil ", ~a ~a"
		 (if (equal imm5 #*00000) "RRX" "ROR")
		 (if (equal imm5 #*00000) "" (u-int imm5))))))

(defun shift-type-to-string (type)
  (cond ((equal type #*00) "LSL")
	((equal type #*01) "LSR")
	((equal type #*10) "ASR")
	((equal type #*11) "ROR")))

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
    ;; Aditional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "ADC~a~a ~a~a, #~a #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1))
	(imm32 (arm-expand-imm imm12)))
    (when condition-passed
      (multiple-value-bind (result carry overflow)
	  (add-with-carry (reg n) imm32 APSR.C)
	(if (= d 15)              ; can only occur for ARM encoding
	    (alu-write-pc result) ; setflags is always false here
	    (progn (setf (reg d) result)
		   (when setflags
		     (setf APSR.N (<> result 31 31))
		     (setf APSR.Z (is-zero-bit result))
		     (setf APSR.C carry)
		     (setf APSR.V overflow))))))))
	    

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
    ;; Aditional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "ADC~a~a ~a~a, ~a~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(let ((shifted (shift (reg m) shift-t shift-n APSR.C)))
	  (multiple-value-bind (result carry overflow)
	      (add-with-carry (reg n) shifted APSR.C)
	    (if (= d 15)              ; can only occur for ARM encoding
		(alu-write-pc result) ; setflags is always false here
		(progn (setf (reg d) result)
		       (when setflags
			 (setf APSR.N (<> result 31 31))
			 (setf APSR.Z (is-zero-bit result))
			 (setf APSR.C carry)
			 (setf APSR.V overflow))))))))))
		     

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
    ;; Aditional condition.
    t
  ;; Disassembler.
  (format stream
	  "ADC~a~a ~a~a, ~a, ~a ~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (shift-type-to-string typ)
	  (register-to-string Rs))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(setflags (equal S #*1))
	(shift-t (decode-reg-shift typ)))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredictable)
    (when condition-passed
      (let* ((shift-n (u-int (<> (reg s) 7 0)))
	     (shifted (shift (reg m) shift-t shift-n APSR.C)))
	(multiple-value-bind (result carry overflow)
	    (add-with-carry (reg n) shifted APSR.C)
	  (setf (reg d) result)
	  (when setflags
	    (setf APSR.N (<> result 31 31))
	    (setf APSR.Z (is-zero-bit result))
	    (setf APSR.C carry)
	    (setf APSR.V overflow)))))))

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
    ;; Aditional condition.
    (not (or (and (equal Rn #*1111) (equal S #*0))
	     (equal Rn #*1101)
	     (and (equal Rd #*1111) (equal S #*1))))
  ;; Disassembler.
  (progn
    (format stream
	    "ADD~a~a ~a~a, #~a #~a"
	    (if (equal S #*1) "S" "")
	    (condition-to-string cond)
	    (optional-register-to-string Rd Rn)
	    (register-to-string Rn)
	    (u-int (<> imm12 7 0))
	    (u-int (<> imm12 11 8))))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1))
	(imm32 (arm-expand-imm imm12)))
    (when condition-passed
      (multiple-value-bind (result carry overflow)
	  (add-with-carry (reg n) imm32 #*0)
	(if (= d 15)
	    (alu-write-pc result) ; setflags is always false here
	    (progn (setf (reg d) result)
		   (when setflags
		     (setf APSR.N (<> result 31 31))
		     (setf APSR.Z (is-zero-bit result))
		     (setf APSR.C carry)
		     (setf APSR.V overflow))))))))

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
    ;; Additional condition.
    (not (or (and (equal Rd #*1111) (equal S #*1))
	     (equal Rn #*1101)))
  ;; Disassembler.
  (format stream
	  "ADD~a~a ~a~a, ~a~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(let ((shifted (shift (reg m) shift-t shift-n APSR.C)))
	  (multiple-value-bind (result carry overflow)
	      (add-with-carry (reg n) shifted #*0)
	    (if (= d 15)              
		(alu-write-pc result) ; setflags is always false here
		(progn (setf (reg d) result)
		       (when setflags
			 (setf APSR.N (<> result 31 31))
			 (setf APSR.Z (is-zero-bit result))
			 (setf APSR.C carry)
			 (setf APSR.V overflow))))))))))

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
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "ADD~a~a ~a~a, ~a, ~a ~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (shift-type-to-string typ)
	  (register-to-string Rs))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(setflags (equal S #*1))
	(shift-t (decode-reg-shift typ)))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredictable)
    (when condition-passed
      (let* ((shift-n (u-int (<> (reg s) 7 0)))
	     (shifted (shift (reg m) shift-t shift-n APSR.C)))
	(multiple-value-bind (result carry overflow)
	    (add-with-carry (reg n) shifted #*0)
	  (setf (reg d) result)
	  (when setflags
	    (setf APSR.N (<> result 31 31))
	    (setf APSR.Z (is-zero-bit result))
	    (setf APSR.C carry)
	    (setf APSR.V overflow)))))))

;;; Instruction: ADD (SP plus immediate)
;;; 
;;; This instruction adds an immediate value to the SP value, and
;;; writes the result to the destination register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 0|S|1 1 0 1|   Rd  |       imm12           |"
    ;; Additional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "ADD~s~s ~sSP, #~a #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd #*1101)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(setflags (equal S #*1))
	(imm32 (arm-expand-imm imm12)))
    (when condition-passed
      (multiple-value-bind (result carry overflow)
	  (add-with-carry (reg 13) imm32 #*0)
	(if (= d 15)
	    (alu-write-pc result) ; setflags is always false here
	    (progn (setf (reg d) result)
		   (when setflags
		     (setf APSR.N (<> result 31 31))
		     (setf APSR.Z (is-zero-bit result))
		     (setf APSR.C carry)
		     (setf APSR.V overflow))))))))


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
    ;; Additional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "ADD~a~a ~aSP, ~a~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd #*1101)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((d (u-int Rd))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(let ((shifted (shift (reg m) shift-t shift-n APSR.C)))
	  (multiple-value-bind (result carry overflow)
	      (add-with-carry (reg #*1101) shifted #*0)
	    (if (= d 15)              
		(alu-write-pc result) ; setflags is always false here
		(progn (setf (reg d) result)
		       (when setflags
			 (setf APSR.N (<> result 31 31))
			 (setf APSR.Z (is-zero-bit result))
			 (setf APSR.C carry)
			 (setf APSR.V overflow))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADR 
;;;
;;; Encoding A1
;;; 
;;; This instruction adds an immediate value to the PC value to form a
;;; PC-relative address, and writes the result to the destination
;;; register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 0|0|1 1 1 1|   Rd  |       imm12           |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "ADD~s ~s, PC, #~a #~a"
	  (condition-to-string cond)
	  (register-to-string Rd)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(imm32 (arm-expand-imm imm12)))
    (when condition-passed
      (let ((result (add (align #*1111 4) imm32)))
	(if (= d 15)
	    (alu-write-pc result)
	    (setf (reg d) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ADR 
;;;
;;; Encoding A2
;;; 
;;; This instruction adds an immediate value to the PC value to form a
;;; PC-relative address, and writes the result to the destination
;;; register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 1 0 0|0|1 1 1 1|   Rd  |       imm12           |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "SUB~s ~s, PC, #~a #~a"
	  (condition-to-string cond)
	  (register-to-string Rd)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(imm32 (arm-expand-imm imm12)))
    (when condition-passed
      (let ((result (sub (align #*1111 4) imm32)))
	(if (= d 15)
	    (alu-write-pc result)
	    (setf (reg d) result))))))

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
    ;; Additional condition.
    (not (and (equal rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "AND~a~a ~a~a, #~a #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1)))
    (multiple-value-bind (imm32 carry)
	(arm-expand-imm-c imm12 APSR.C)
      (when condition-passed
	(let ((result (bit-and (reg n) imm32)))
	  (if (= d 15)
	      (alu-write-pc result) ; setflags is always false here
	      (progn (setf (reg d) result)
		     (when setflags
		       (setf APSR.N (<> result 31 31))
		       (setf APSR.Z (is-zero-bit result))
		       (setf APSR.C carry)))))))))  ; APSR.V unchanged
    
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
    ;; Additional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "AND~a~a ~a~a, ~a~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(multiple-value-bind (shifted carry)
	    (shift-c (reg m) shift-t shift-n APSR.C)
	  (let ((result (bit-and (reg n) shifted)))
	    (if (= d 15)              
		(alu-write-pc result) ; setflags is always false here
		(progn (setf (reg d) result)
		       (when setflags
			 (setf APSR.N (<> result 31 31))
			 (setf APSR.Z (is-zero-bit result))
			 (setf APSR.C carry)))))))))) ; APSR.V unchanged

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
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "AND~a~a ~a~a, ~a, ~a ~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (shift-type-to-string typ)
	  (register-to-string Rs))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(setflags (equal S #*1))
	(shift-t (decode-reg-shift typ)))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredictable)
    (when condition-passed
      (let ((shift-n (u-int (<> (reg s) 7 0))))
	(multiple-value-bind (shifted carry)
	    (shift-c (reg m) shift-t shift-n APSR.C)
	  (let ((result (bit-and (reg n) shifted)))
	    (setf (reg d) result)
	    (when setflags
	      (setf APSR.N (<> result 31 31))
	      (setf APSR.Z (is-zero-bit result))
	      (setf APSR.C carry))))))))  ; APSR.V unchanged

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
    ;; Additional condition.
    (not (and (equal Rd #*1111)
	      (equal S #*1)))
  ;; Dissassembler.
  (format stream
	  "ASR~a~a ~a~a, #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rm)
	  (register-to-string Rm)
	  (if (equal imm5 #*00000) 32 (u-int imm5)))
  ;; Operation.
  (let ((d (u-int Rd))
	(m (u-int Rm))
	(setflags (equal S #*1))
	(shift-n (nth-value 1 (decode-imm-shift #*10 imm5))))
    (when condition-passed
      (multiple-value-bind (result carry)
	  (shift-c (reg m) :sr-type-asr shift-n APSR.C)
	(if (= d 15)
	    (alu-write-pc result) ; setflags is always false here.
	    (progn (setf (reg d) result)
		   (when setflags
		     (setf APSR.N (<> result 31 31))
		     (setf APSR.Z (is-zero-bit result))
		     (setf APSR.C carry)))))))) ; APSR.V unchanged.

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
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "ASR~a~a ~a~a, ~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm))
  ;; Operation.
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredictable)
    (when condition-passed
      (let ((shift-n (u-int (<> (reg m) 7 0))))
	(multiple-value-bind (result carry)
	    (shift-c (reg n) :sr-type-asr shift-n APSR.C)
	  (setf (reg d) result)
	  (when setflags
	    (setf APSR.N (<> result 31 31))
	    (setf APSR.Z (is-zero-bit result))
	    (setf APSR.C carry))))))) ; APSR.V unchanged.
    
;;; Instruction: B
;;; 
;;; Branch causes a branch to a target address.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |1 0 1 0|                   imm24                       |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "B~a ~a"
	  (condition-to-string cond)
	  (s-int (sign-extend (cat imm24 #*00) 32)))
  ;; Operation
  (let ((imm32 (sign-extend (cat imm24 #*00) 32)))
    (when condition-passed
      (branch-write-pc (add (reg 15) imm32)))))

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
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "BFC~a ~a, #~a #~a"
	  (condition-to-string cond)
	  (register-to-string Rd)
	  (u-int lsb)
	  (1+ (- (u-int msb) (u-int lsb))))
  ;; Operation
  (let ((d (u-int Rd))
	(msbit (u-int msb))
	(lsbit (u-int lsb)))
    (when (= d 15)
      unpredictable)
    (when condition-passed
      (if (>= msbit lsbit)
	  (setf (<> (reg d) msbit lsbit)
		(replicate #*0 (1+ (- msbit lsbit))))
	  unpredictable))))

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
    ;; Additional condition.
    (not (equal Rd #*1111))
  ;; Disassembler.
  (format stream
	  "BFI~a ~a, ~a, #~a #~a"
	  (condition-to-string cond)
	  (register-to-string Rd)
	  (register-to-string Rn)
	  (u-int lsb)
	  (1+ (- (u-int msb) (u-int lsb))))
  ;; Operation.
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(msbit (u-int msb))
	(lsbit (u-int lsb)))
    (when (= d 15)
      unpredictable)
    (when condition-passed
      (if (>= msbit lsbit)
	  (setf (<> (reg d) msbit lsbit)
		(<> (reg n) (- msbit lsbit) 0))
	  unpredictable))))

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
  ;; Disassembler
  (format stream
	  "BIC~a~a ~a~a, #~a #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1)))
    (multiple-value-bind (imm32 carry)
	(arm-expand-imm-c imm12 APSR.C)
      (when condition-passed
	(let ((result (bit-andc2 (reg n) imm32)))
	  (if (= d 15)
	      (alu-write-pc result) ; setflags is always false here
	      (progn (setf (reg d) result)
		     (when setflags
		       (setf APSR.N (<> result 31 31))
		       (setf APSR.Z (is-zero-bit result))
		       (setf APSR.C carry)))))))))  ; APSR.V unchanged

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
    ;; Additional condition.
    t
  ;; Disassembler.
  (progn (when (not (equal cond #*1110))
	   unpredictable)
	 (format stream
		 "BKPT #~a"
		 (zero-extend (cat imm12 imm4) 32)))
  ;; Operation
  (let ((imm32 (zero-extend (cat imm12 imm4) 32)))
    (when (not (equal cond #*1110))
      unpredictable)
    ;; FIXME: do something more reasonable here later.
    (throw 'bkpt imm32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: BL (immediate)
;;;
;;; Branch with Link calls a subroutine at a PC-relative address.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |1 0 1 1|                   imm24                       |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "BL~a ~a"
	  (condition-to-string cond)
	  (sign-extend (cat imm24 #*00) 32))
  ;; Operation
  (let ((imm32 (sign-extend (cat imm24 #*00) 32)))
    (when condition-passed
      (setf (reg 14) (sub (reg 15) 4))
      (let ((target-address (add (align (reg 15) 4) imm32)))
	(branch-write-pc target-address)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CMP (immediate)
;;; 
;;; Compare (immediate) subtracts an immediate value from a register
;;; value. It updates the condition flags based on the result, and
;;; discards the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|1 0 1 0|1|  Rn   |0 0 0 0|          imm12        |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "CMP~a ~a, #~a, #~a"
	  (condition-to-string cond)
	  (register-to-string Rn)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((n (u-int Rn))
	(imm32 (arm-expand-imm imm12)))
    (when condition-passed
      (multiple-value-bind (result carry overflow)
	  (add-with-carry (reg n) (bit-not imm32) #*1)
	(setf APSR.N (<> result 31 31))
	(setf APSR.Z (is-zero-bit result))
	(setf APSR.C carry)
	(setf APSR.V overflow)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CMP (register)
;;; 
;;; Compare (register) subtracts an optionally-shifted register value
;;; from a register value. It updates the condition flags based on the
;;; result, and discards the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|1 0 1 0|1|  Rn   |0 0 0 0|  imm5   |typ|0|  Rm   |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "CMP~s ~s, ~s~s"
	  (condition-to-string cond)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((n (u-int Rn))
	(m (u-int Rm)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(let ((shifted (shift (reg m) shift-t shift-n APSR.C)))
	  (multiple-value-bind (result carry overflow)
	      (add-with-carry (reg n) (bit-not shifted) #*1)
	    (setf APSR.N (<> result 31 31))
	    (setf APSR.Z (is-zero-bit result))
	    (setf APSR.C carry)
	    (setf APSR.V overflow)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: CMP (registser-shifted register)
;;; 
;;; Compare (register-shifted register) subtracts a register-shifted
;;; register value from a register value. It updates the condition
;;; flags based on the result, and discards the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|1 0 1 0|1|  Rn   |0 0 0 0|  Rs   |0|typ|1|  Rm   |"
    ;; Additional contition.
    t
  ;; Disassembler
  (format stream
	  "CMP~a ~a, ~a, ~a ~a"
	  (condition-to-string cond)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (shift-type-to-string typ)
	  (register-to-string Rs))
  ;; Operation.
  (let ((n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(shift-t (decode-reg-shift typ)))
    (when (or (= n 15) (= m 15) (= s 15))
      unpredictable)
    (when condition-passed
      (let* ((shift-n (u-int (<> (reg s) 7 0)))
	     (shifted (shift (reg m) shift-t shift-n APSR.C)))
	(multiple-value-bind (result carry overflow)
	    (add-with-carry (reg n) (bit-not shifted) #*1)
	  (setf APSR.N (<> result 31 31))
	  (setf APSR.Z (is-zero-bit result))
	  (setf APSR.C carry)
	  (setf APSR.V overflow))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: EOR (immediate)
;;; 
;;; This instruction performs a bitwise exclusive OR of a register
;;; value and an immediate value, and writes the result to the
;;; destination register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|0 0 0 1|S|   Rn  |   Rd  |       imm12           |"
    ;; Additional condition.
    (not (and (equal rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "EOR~a~a ~a~a, #~a #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1)))
    (multiple-value-bind (imm32 carry)
	(arm-expand-imm-c imm12 APSR.C)
      (when condition-passed
	(let ((result (bit-xor (reg n) imm32)))
	  (if (= d 15)
	      (alu-write-pc result) ; setflags is always false here
	      (progn (setf (reg d) result)
		     (when setflags
		       (setf APSR.N (<> result 31 31))
		       (setf APSR.Z (is-zero-bit result))
		       (setf APSR.C carry)))))))))  ; APSR.V unchanged
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: EOR (register)
;;; 
;;; This instruction performs a bitwise exclusive OR of a register
;;; value and an optionally-shifted register value, and writes the
;;; result to the destination register. It can optionally update the
;;; condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 0 0 1|S|   Rn  |   Rd  |   imm5  |typ|0|  Rm   |"
    ;; Additional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "EOR~a~a ~a~a, ~a~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(multiple-value-bind (shifted carry)
	    (shift-c (reg m) shift-t shift-n APSR.C)
	  (let ((result (bit-xor (reg n) shifted)))
	    (if (= d 15)              
		(alu-write-pc result) ; setflags is always false here
		(progn (setf (reg d) result)
		       (when setflags
			 (setf APSR.N (<> result 31 31))
			 (setf APSR.Z (is-zero-bit result))
			 (setf APSR.C carry)))))))))) ; APSR.V unchanged

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: EOR (register-shifted register)
;;; 
;;; This instruction performs a bitwise exclusive OR of a register
;;; value and a register-shifted register value. It writes the result
;;; to the destination register, and can optionally update the
;;; condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|0 0 0 1|S|   Rn  |   Rd  |   Rs  |0|typ|1|  Rm   |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "EOR~a~a ~a~a, ~a, ~a ~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (shift-type-to-string typ)
	  (register-to-string Rs))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(setflags (equal S #*1))
	(shift-t (decode-reg-shift typ)))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredictable)
    (when condition-passed
      (let ((shift-n (u-int (<> (reg s) 7 0))))
	(multiple-value-bind (shifted carry)
	    (shift-c (reg m) shift-t shift-n APSR.C)
	  (let ((result (bit-xor (reg n) shifted)))
	    (setf (reg d) result)
	    (when setflags
	      (setf APSR.N (<> result 31 31))
	      (setf APSR.Z (is-zero-bit result))
	      (setf APSR.C carry))))))))  ; APSR.V unchanged

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
    ;; Additional condition.
    (not (or (equal Rn #*1111)
	     (and (equal Rn #*1101)
		  (equal U #*1)
		  (equal imm12 #*000000000100))))
  ;; Disassembler.
  (format stream
	  "LDR~a ~a, [~a], #~a~a"
	  (condition-to-string cond)
	  (u-int Rt)
	  (u-int Rn)
	  (if (equal U #*0) "-" "")
	  (u-int imm12))
  ;; Operation.
  (let ((tt (u-int Rt))
	(n (u-int Rn))
	(imm32 (zero-extend imm12 32))
	(add (equal U #*1)))
    (when (= n tt)
      unpredictable)
    (when condition-passed
      (let* ((offset-addr (if add (add (reg n) imm32) (sub (reg n) imm32)))
	     (address (reg n))
	     (data (mem-u address 4)))
	(setf (reg n) offset-addr)
	(if (= tt 15)
	    (if (equal (<> address 1 0) #*00)
		(load-write-pc data)
		unpredictable)
	    (setf (reg tt) data))))))

;;; P = 1 & W = 0
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|0|1|  Rn   |  Rt   |         imm12         |"
    ;; Additional condition.
    (not (equal Rn #*1111))
  ;; Disassembler
  (format stream
	  "LDR~a ~a, [~a~a]"
	  (condition-to-string cond)
	  (register-to-string Rt)
	  (register-to-string Rn)
	  (if (and (equal U #*1) (equal imm12 #*000000000000))
	      ""
	      (format nil ", #~a~a"
		      (if (equal U #*1) "" "-")
		      (u-int imm12))))
  ;; Operation
  (let ((tt (u-int Rt))
	(n (u-int Rn))
	(imm32 (zero-extend imm12 32))
	(add (equal U #*1)))
    (when condition-passed
      (let* ((offset-addr (if add (add (reg n) imm32) (sub (reg n) imm32)))
	     (address offset-addr)
	     (data (mem-u address 4)))
	(if (= tt 15)
	    (if (equal (<> address 1 0) #*00)
		(load-write-pc data)
		unpredictable)
	    (setf (reg tt) data))))))

;;; P = 1 & W = 1
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|1|1|  Rn   |  Rt   |         imm12         |"
    ;; Additional condition.
    (not (equal Rn #*1111))
  ;; Disassembler.
  (format stream
	  "LDR~a ~a, [~a, #~a~a]!"
	  (condition-to-string cond)
	  (register-to-string Rt)
	  (register-to-string Rn)
	  (if (equal U #*1) "" "-")
	  (u-int imm12))
  ;; Operation.
  (let ((tt (u-int Rt))
	(n (u-int Rn))
	(imm32 (zero-extend imm12 32))
	(add (equal U #*1)))
    (when (= n tt)
      unpredictable)
    (when condition-passed
      (let* ((offset-addr (if add (add (reg n) imm32) (sub (reg n) imm32)))
	     (address offset-addr)
	     (data (mem-u address 4)))
	(setf (reg n) offset-addr)
	(if (= tt 15)
	    (if (equal (<> address 1 0) #*00)
		(load-write-pc data)
		unpredictable)
	    (setf (reg tt) data))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ORR (immediate)
;;; 
;;; This instruction performs a bitwise (inclusive) OR of a register
;;; value and an immediate value, and writes the result to the
;;; destination register.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|1|1 1 0 0|S|   Rn  |   Rd  |       imm12           |"
    ;; Additional condition.
    (not (and (equal rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "ORR~a~a ~a~a, #~a #~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (u-int (<> imm12 7 0))
	  (u-int (<> imm12 11 8)))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(setflags (equal S #*1)))
    (multiple-value-bind (imm32 carry)
	(arm-expand-imm-c imm12 APSR.C)
      (when condition-passed
	(let ((result (bit-ior (reg n) imm32)))
	  (if (= d 15)
	      (alu-write-pc result) ; setflags is always false here
	      (progn (setf (reg d) result)
		     (when setflags
		       (setf APSR.N (<> result 31 31))
		       (setf APSR.Z (is-zero-bit result))
		       (setf APSR.C carry)))))))))  ; APSR.V unchanged
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ORR (register)
;;; 
;;; This instruction performs a bitwise (inclusive) OR of a register
;;; value and an optionally-shifted register value, and writes the
;;; result to the destination register. It can optionally update the
;;; condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|1 1 0 0|S|   Rn  |   Rd  |   imm5  |typ|0|  Rm   |"
    ;; Additional condition.
    (not (and (equal Rd #*1111) (equal S #*1)))
  ;; Disassembler.
  (format stream
	  "ORR~a~a ~a~a, ~a~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (optional-shift-of-register typ imm5))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(setflags (equal S #*1)))
    (multiple-value-bind (shift-t shift-n)
	(decode-imm-shift typ imm5)
      (when condition-passed
	(multiple-value-bind (shifted carry)
	    (shift-c (reg m) shift-t shift-n APSR.C)
	  (let ((result (bit-ior (reg n) shifted)))
	    (if (= d 15)              
		(alu-write-pc result) ; setflags is always false here
		(progn (setf (reg d) result)
		       (when setflags
			 (setf APSR.N (<> result 31 31))
			 (setf APSR.Z (is-zero-bit result))
			 (setf APSR.C carry)))))))))) ; APSR.V unchanged

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: ORR (register-shifted register)
;;; 
;;; This instruction performs a bitwise (inclusive) OR of a register
;;; value and a register-shifted register value. It writes the result
;;; to the destination register, and can optionally update the
;;; condition flags based on the result.
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 0|0|1 1 0 0|S|   Rn  |   Rd  |   Rs  |0|typ|1|  Rm   |"
    ;; Additional condition.
    t
  ;; Disassembler.
  (format stream
	  "ORR~a~a ~a~a, ~a, ~a ~a"
	  (if (equal S #*1) "S" "")
	  (condition-to-string cond)
	  (optional-register-to-string Rd Rn)
	  (register-to-string Rn)
	  (register-to-string Rm)
	  (shift-type-to-string typ)
	  (register-to-string Rs))
  ;; Operation
  (let ((d (u-int Rd))
	(n (u-int Rn))
	(m (u-int Rm))
	(s (u-int Rs))
	(setflags (equal S #*1))
	(shift-t (decode-reg-shift typ)))
    (when (or (= d 15) (= n 15) (= m 15))
      unpredictable)
    (when condition-passed
      (let ((shift-n (u-int (<> (reg s) 7 0))))
	(multiple-value-bind (shifted carry)
	    (shift-c (reg m) shift-t shift-n APSR.C)
	  (let ((result (bit-ior (reg n) shifted)))
	    (setf (reg d) result)
	    (when setflags
	      (setf APSR.N (<> result 31 31))
	      (setf APSR.Z (is-zero-bit result))
	      (setf APSR.C carry))))))))  ; APSR.V unchanged

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction: STR (immediate)
;;;
;;; Store Register (immediate) calculates an address from a base
;;; register value and an immediate offset, and stores a word from a
;;; register to memory. It can use offset, post-indexed, or
;;; pre-indexed addressing.
;;;
;;; To get the assembly syntax closer to the ARM standard, we encode
;;; this instruction as three different instance: 
;;; P = 0 & W = 0; P = 1 & W = 0; P = 1 & W = 1

;;; P = 0 & W = 0
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|0|U|0|0|0|  Rn   |  Rt   |         imm12         |"
    ;; Additional condition.
    t
  ;; Disassemler.
  (format stream
	  "STR~a ~a, [~a], #~a~a"
	  (condition-to-string cond)
	  (u-int Rt)
	  (u-int Rn)
	  (if (equal U #*0) "-" "")
	  (u-int imm12))
  ;; Operation.
  (let ((tt (u-int Rt))
	(n (u-int Rn))
	(imm32 (zero-extend imm12 32))
	(add (equal U #*1)))
    (when (or (= n 15) (= n tt))
      unpredictable)
    (when condition-passed
      (let* ((offset-addr (if add (add (reg n) imm32) (sub (reg n) imm32)))
	     (address (reg n)))
	(setf (mem-u address 4)
	      (if (= tt 15)
		  (pc-store-value)
		  (reg tt)))
	(setf (reg n) offset-addr)))))

;;; P = 1 & W = 0
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|0|0|  Rn   |  Rt   |         imm12         |"
    ;; Additional condition.
    t
  ;; Disassemler.
  (format stream
	  "STR~a ~a, [~a~a]"
	  (condition-to-string cond)
	  (register-to-string Rt)
	  (register-to-string Rn)
	  (if (and (equal U #*1) (equal imm12 #*000000000000))
	      ""
	      (format nil ", #~a~a"
		      (if (equal U #*1) "" "-")
		      (u-int imm12))))
  ;; Operation.
  (let ((tt (u-int Rt))
	(n (u-int Rn))
	(imm32 (zero-extend imm12 32))
	(add (equal U #*1)))
    (when condition-passed
      (let* ((offset-addr (if add (add (reg n) imm32) (sub (reg n) imm32)))
	     (address offset-addr))
	(setf (mem-u address 4)
	      (if (= tt 15)
		  (pc-store-value)
		  (reg tt)))))))


;;; P = 1 & W = 1
(define-instruction
    ;;3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 
    ;;1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
    "| cond  |0 1 0|1|U|0|1|0|  Rn   |  Rt   |         imm12         |"
    ;; Additional condition.
    (not (and (equal Rn #*1101) (equal U #*0) (equal imm12 #*000000000100)))
  ;; Disassemler.
  (format stream
	  "STR~a ~a, [~a, #~a~a]"
	  (condition-to-string cond)
	  (register-to-string Rt)
	  (register-to-string Rn)
	  (if (equal U #*1) "" "-")
	  (u-int imm12))
  ;; Operation.
  (let ((tt (u-int Rt))
	(n (u-int Rn))
	(imm32 (zero-extend imm12 32))
	(add (equal U #*1)))
    (when (or (= n 15) (= n tt))
      unpredictable)
    (when condition-passed
      (let* ((offset-addr (if add (add (reg n) imm32) (sub (reg n) imm32)))
	     (address offset-addr))
	(setf (mem-u address 4)
	      (if (= tt 15)
		  (pc-store-value)
		  (reg tt)))
	(setf (reg n) offset-addr)))))
