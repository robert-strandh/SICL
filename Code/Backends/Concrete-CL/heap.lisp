(in-package #:sicl-exp-heap)

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

;;; The size of memory in bytes.
(defconstant +size-of-memory+ 80000000)

(defparameter *memory* (make-array +size-of-memory+
				   :element-type '(unsigned-byte 8)
				   :initial-element #xe0))

(defun initialize-memory ()
  (loop for i from 0 below +size-of-memory+
	do (setf (aref *memory* i) #xe0)))

(defparameter *heap-pointer* 0)

;;; In this version, we assume a little-endian byte order.
(defun memref (address)
  (declare (type word address))
  (assert (zerop (mod address +word-size-in-bytes+)))
  (let ((result 0))
    (loop for a from address
	  for shift from 0 by 8
	  repeat +word-size-in-bytes+
	  do (setf result
		   (logior result
			   (ash (aref *memory* a) shift))))
    result))

(defun memset (address value)
  (declare (type word address value))
  (assert (zerop (mod address +word-size-in-bytes+)))
  (loop for a from address
	repeat +word-size-in-bytes+
	do (setf (aref *memory* a)
		 (logand value #xff))
	   (setf value (ash value -8))))

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
    (when (> *heap-pointer* +size-of-memory+)
      (error "heap exhausted"))))

(defun malloc-words (size-in-words)
  (let ((result (malloc (* size-in-words +word-size-in-bytes+))))
    (loop for address from result by +word-size-in-bytes+
	  repeat size-in-words
	  do (memset address #b0101010101010101))
    result))

(defun initialize-heap ()
  (setf *allocation-points* (make-hash-table))
  (initialize-memory)
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
		     

