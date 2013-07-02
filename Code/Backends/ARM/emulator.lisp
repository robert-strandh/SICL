(in-package #:sicl-arm-assembler)

(defclass purpose ()
  ())

;;; Number of bytes in a page.  The exact value of this constant is
;;; not that important, because we divide the memory into pages just
;;; so that we won't have to allocate a vector of size 2^32 right
;;; away.
(defconstant +page-size+ #.(expt 2 16))

(defconstant +number-of-pages+ (/ #.(expt 2 32) +page-size+))

(defun ensure-page (memory page-number)
  (when (null (aref memory page-number))
    (setf (aref memory page-number)
	  (make-array +page-size+
		      :element-type '(unsigned-byte 8)
		      :initial-element 0))))

(defun load-byte (memory address)
  (multiple-value-bind (page-number offset)
      (floor address +page-size+)
    (ensure-page memory page-number)
    (aref (aref memory page-number) offset)))

(defun store-byte (memory address value)
  (multiple-value-bind (page-number offset)
      (floor address +page-size+)
    (ensure-page memory page-number)
    (setf (aref (aref memory page-number) offset) value)))
  
(defun load-memory-unaligned (memory address number-of-bytes)
  (loop for offset from 0 below number-of-bytes
	for bit-pos = (* offset 8)
	sum (ash (load-byte memory (+ address offset)) bit-pos)))

(defun store-memory-unaligned (memory address value number-of-bytes)
  (loop for offset from 0 below number-of-bytes
	for bit-pos = (* offset 8)
	do (store-byte memory (+ address offset) (ldb (byte 8 bit-pos) value))))

(defun load-word (memory address)
  (assert (= (mod address 4) 0))
  (load-memory-unaligned memory address 4))

(defun store-word (memory address value)
  (assert (= (mod address 4) 0))
  (store-byte memory (+ address 0) (ldb (byte 8 0) value))
  (store-byte memory (+ address 1) (ldb (byte 8 8) value))
  (store-byte memory (+ address 2) (ldb (byte 8 16) value))
  (store-byte memory (+ address 3) (ldb (byte 8 24) value)))

(defclass emulation (purpose)
  ((%registers
    :initform (make-array 16
			  :element-type '(bit-vector 32)
			  :initial-contents
			  (loop repeat 16
				collect (make-array 32
						    :element-type 'bit
						    :initial-element 0)))
    :reader registers)
   (%condition-codes
    :initform (make-array 32 :element-type 'bit :initial-element 0)
    :reader condition-codes)
   (%current-instruction
    :initform nil
    :accessor current-instruction)
   (%next-instruction
    :initform nil
    :accessor next-instruction)
   (%memory
    :initform (make-array +number-of-pages+ :initial-element nil)
    :reader memory)))

(defun execute-one-instruction (emulator)
  (let ((*machine* emulator))
    (when (null (current-instruction emulator))
      ;; The current instruction will be NIL when the machine is
      ;; started, but also right after a branchinstruction has been
      ;; executed.  When that is the case, we need to start loading
      ;; instructions from the address in PC.
      (setf (current-instruction emulator)
	    (integer-to-bit-vector
	     (load-word (memory emulator) (u-int (reg 15)))
	     32))
      (setf (reg 15) (add (reg 15) 4))
      (setf (next-instruction emulator)
	    (integer-to-bit-vector
	     (load-word (memory emulator) (u-int (reg 15)))
	     32))
      (setf (reg 15) (add (reg 15) 4)))
    ;; Executing a branch instruction will set the next
    ;; instruction to NIL.
    (execute-instruction (current-instruction emulator))
    ;; So if the instruction we just executed was a branch, we are now
    ;; setting the current instruction to NIL,
    (setf (current-instruction emulator)
	  (next-instruction emulator))
    ;; If the instruction that we just exedcuted was a branch, the
    ;; instruction that gets loaded here will be discared.
    (setf (next-instruction emulator)
	  (integer-to-bit-vector
	   (load-word (memory emulator) (u-int (reg 15)))
	   32))
    (setf (reg 15) (add (reg 15) 4))))

	    
