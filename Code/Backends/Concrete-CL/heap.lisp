(in-package #:sicl-exp-heap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word.
;;;
;;; A word is the machine representation of any data item.
;;; It can be interpreted differently in different situation. 
;;; As far as the host system is concerned, a word is a positive 
;;; number between 0 and 2^n-1 where n is the number of bits in
;;; the word.  To represent negative numbers, we use 2-complement.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +word-size+ 64)
  (defconstant +word-size-in-bytes+ (/ +word-size+ 8)))

(deftype word () `(unsigned-byte ,+word-size+))

(defun word-from-unsigned-host-number (host-number)
  (assert (<= 0
	      host-number
	      (1- (ash 1 +word-size+))))
  host-number)
	  

(defun word-from-signed-host-number (host-number)
  (assert (<= (- (ash 1 (1- +word-size+)))
	      host-number
	      (1- (ash 1 (1- +word-size+)))))
  (if (minusp host-number)
      (+ host-number (ash 1 +word-size+))
      host-number))

(defun signed-host-number-from-word (word)
  (assert (typep word 'word))
  (if (>= word (ash 1 (1- +word-size+)))
      (- word (ash 1 +word-size+))
      word))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Heap management.
;;; 
;;; The heap is just a bunch of consecutive words.  We assume 
;;; a byte-addressed heap.  We also assume that when an address
;;; is used to fetch or store a word on the heap, then the 
;;; address is a multiple of the number of bytes in a word.  This
;;; assumption helps us debug problems where tagged objects are
;;; accidentally used as addresses.  
;;;
;;; At the moment, there is no garbage collection.  Allocation is
;;; done linearly from 0 and up.  

(defparameter *heap* (make-array 10000000 :initial-element 0))

(defparameter *heap-pointer* 0)

;;; Since we represent the heap as a Common Lisp vector, each
;;; element of which represents a word, we must translate every
;;; address to an index of the vector representing the heap.  
;;; When doing that, we also verify that the address we are given
;;; is indeed a multiple of the number of words in a byte. 
(defun address-to-heap-index (address)
  (assert (zerop (mod address +word-size-in-bytes+)))
  (ash address #.(- (round (log +word-size-in-bytes+ 2)))))

(defun memref (address)
  (assert (< address *heap-pointer*))
  (aref *heap* (address-to-heap-index address)))

(defun memset (address value)
  (assert (< address *heap-pointer*))
  (assert (typep value 'word))
  (setf (aref *heap* (address-to-heap-index address)) value))

;;; For debugging purposes, we remember the start address of 
;;; each allocation unit, so that we have some idea of the 
;;; frontiers between objects on the heap.  
(defunbound *allocation-points*)

(defun malloc (size-in-bytes)
  (assert (typep size-in-bytes 'unsigned-byte))
  (setf (gethash *heap-pointer* *allocation-points*)
	size-in-bytes)
  (prog1 *heap-pointer*
    ;; round up to next word
    (incf *heap-pointer* 
	  (* +word-size-in-bytes+
	     (ceiling size-in-bytes +word-size-in-bytes+)))
    (when (> *heap-pointer* (length *heap*))
      (error "heap exhausted"))))

(defun malloc-words (size-in-words)
  (let ((result (malloc (* size-in-words +word-size-in-bytes+))))
    (loop for address from result by +word-size-in-bytes+
	  repeat size-in-words
	  do (memset address #b0101010101010101))
    result))

(defun initialize-heap ()
  (setf *allocation-points* (make-hash-table))
  (loop for i from 0 below (length *heap*)
	do (setf (aref *heap* i) #xe0e0e0e))
  (setf *heap-pointer* 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Debugging tools

(defun dump-heap (&optional (n *heap-pointer*) interpret)
  (setf n (min n *heap-pointer*))
  (format *trace-output* "Heap pointer: ~10d    ~16,'0x~%" *heap-pointer* *heap-pointer*)
  (loop for i from 0 below n by +word-size-in-bytes+
	do (when (gethash i *allocation-points*)
	     (format *trace-output* "~%"))
	do (format *trace-output* "~10d  ~8,'0x :: ~16,'0x  ~s~%"
		   i i (memref i)
		   (if interpret
		       (funcall interpret (memref i))
		       ""))))
		     

