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
  
(defun load-word (memory address)
  (assert (= (mod address 4) 0))
  (+ (ash (load-byte memory (+ address 0)) 0)
     (ash (load-byte memory (+ address 1)) 8)
     (ash (load-byte memory (+ address 2)) 16)
     (ash (load-byte memory (+ address 3)) 24)))

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
			  :initial-element
			  #*00000000000000000000000000000000)
    :reader registers)
   (%condition-codes
    :initform (make-array 32 :element-type 'bit :initial-element 0)
    :reader condition-codes)
   (%memory
    :initform (make-array +number-of-pages+ :initial-element nil)
    :reader memory)))
