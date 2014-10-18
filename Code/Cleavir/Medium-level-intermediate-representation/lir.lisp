(cl:in-package #:cleavir-mir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions.

(define-condition input-inputs-mutually-exclusive (error)
  ())

(define-condition both-individual-inputs-must-be-given (error)
  ())

(define-condition successor-successors-mutually-exclusive (error)
  ())

(define-condition both-or-no-individual-successors-must-be-given (error)
  ())

(define-condition successor1-must-be-given (error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

(defun construct-inputs (i i-p i1 i1-p i2 i2-p)
  (cond (i-p
	 (when  (or i1-p i2-p)
	   (error 'input-inputs-mutually-exclusive))
	 i)
	((or (not i1-p) (not i2-p))
	 (error 'both-individual-inputs-must-be-given))
	(t
	 (list i1 i2))))

(defun construct-successors (s s-p s1 s1-p s2 s2-p)
  (cond (s-p
	 (when  (or s1-p s2-p)
	   (error 'successor-successors-mutually-exclusive))
	 s)
	((or (and s1-p (not s2-p))
	     (and (not s1-p) s2-p))
	 (error 'both-or-no-individual-successors-must-be-given))
	(t
	 (list s1 s2))))

(defun construct-successors-bis (s s-p s1 s1-p s2 s2-p)
  (cond (s-p
	 (when  (or s1-p s2-p)
	   (error 'successor-successors-mutually-exclusive))
	 s)
	((and (not s1-p) s2-p)
	 (error 'successor1-must-be-given))
	(s2-p
	 (list s1 s2))
	(t
	 (list s1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Data.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RAW-DATUM.
;;;
;;; This class is the base class for all raw data.  It contains a size
;;; attribute that determines the number of bits that this datum
;;; consists of.

(defclass raw-datum (datum)
  ((%size :initarg :size :reader size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum RAW-UNSIGNED-INTEGER.

(defclass raw-unsigned-integer (raw-datum)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum RAW-SIGNED-INTEGER.

(defclass raw-signed-integer (raw-datum)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum RAW-FLOAT.

(defclass raw-float (raw-datum)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction MEMREF1-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes a single input
;;; containing the address of the word to load.  It has a single
;;; output which is set to the contents of the memory location at the
;;; address.

(defclass memref1-instruction (instruction one-successor-mixin)
  ())

(defun make-memref1-instruction (input output &optional successor)
  (make-instance 'memref1-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction MEMREF1-INSTRUCTION
;;;
;;; This instruction loads a memory location.  It takes a two inputs.
;;; The first input contains the base address of the datum to load.
;;; The second input contains constant offset.  This instruction has a
;;; single output which is set to the contents of the memory location
;;; at the address computed as the sum of the base address and the
;;; offset.

(defclass memref2-instruction (instruction one-successor-mixin)
  ())

(defun make-memref2-instruction (input1 input2 output &optional successor)
  (make-instance 'memref2-instruction
    :inputs (list input1 input2)
    :outputs (list output)
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction MEMSET1-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; two inputs.  The first input is the address of a location in
;;; memory.  The second input is the item to be stored in that
;;; location.

(defclass memset1-instruction (instruction one-successor-mixin)
  ())

(defun make-memset1-instruction (input1 input2 &optional successor)
  (make-instance 'memset1-instruction
    :inputs (list input1 input2)
    :outputs '()
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction MEMSET2-INSTRUCTION
;;;
;;; This instruction stores an item in a memory location.  It takes
;;; three inputs.  The first input is the base address of a location
;;; in memory.  The second input is an offset.  The third input is the
;;; item to be stored in the location in memory whose address is the
;;; sum of the base address and the offset.

(defclass memset2-instruction (instruction one-successor-mixin)
  ())

(defun make-memset2-instruction (input1 input2 input3 &optional successor)
  (make-instance 'memset2-instruction
    :inputs (list input1 input2 input3)
    :outputs '()
    :successors (if (null successor) '() (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction SIGNED-LESS-INSTRUCTION

(defclass signed-less-instruction (instruction two-successors-mixin)
  ())

(defun make-signed-less-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-less-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction SIGNED-NOT-GREATER-INSTRUCTION

(defclass signed-not-greater-instruction (instruction two-successors-mixin)
  ())

(defun make-signed-not-greater-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-not-greater-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LIR instruction EQUAL-INSTRUCTION

(defclass signed-equal-instruction (instruction two-successors-mixin)
  ())

(defun make-signed-equal-instruction
    (&key
       ((:input1 i1) nil i1-p)
       ((:input2 i2) nil i2-p)
       ((:inputs i) nil i-p)
       ((:successor1 s1) nil s1-p)
       ((:successor2 s2) nil s2-p)
       ((:successors s) nil s-p))
  (make-instance 'signed-equal-instruction
    :inputs (construct-inputs i i-p i1 i1-p i2 i2-p)
    :successors (construct-successors s s-p s1 s1-p s2 s2-p)))
