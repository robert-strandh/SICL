(in-package :sicl.backends.cl)

;;; The Common Lisp backend for the SICL Common Lisp system.
;;; We simualte memory of a real machine by a large vector of
;;; machine words. 

;;; The size of memory in machine words.  
;;; Make this small-ish for now.
(cl:defparameter +memory-size+ 10000000)

;;; The endianness of the machine
(cl:defparameter +endianness+ :little-endian)

;;; The memory that holds every object in the world
(cl:defparameter *memory*
  (cl:make-array +memory-size+ :element-type 'machine-word))

;;; We have no GC yet.  Just allocate in *memory* starting at
;;; the index indicated by this array index.
(cl:defparameter *free-pointer* 0)

;;; Allocate a number of consecutive machine words, and return
;;; a raw address to the beginning of the block allocated.
;;; Do this better by abstracting out the number of tag bits. 
(cl:defun allocate-words (n)
  (cl:prog1 (cl:ash *free-pointer* +number-of-low-tag-bits+)
    (cl:incf *free-pointer* n)))

;;; Load word from memory.
(cl:defun load-word (pointer)
  (cl:check-type pointer machine-word)
  (cl:assert (cl:zerop (cl:logand pointer +low-tag-mask+)))
  (cl:aref *memory* (cl:ash pointer (cl:- +number-of-low-tag-bits+))))

;;; Store word in memory.
(cl:defun store-word (pointer value)
  (cl:check-type pointer machine-word)
  (cl:check-type value machine-word)
  (cl:assert (cl:zerop (cl:logand pointer +low-tag-mask+)))
  (cl:setf (cl:aref *memory* (cl:ash pointer (cl:- +number-of-low-tag-bits+)))
	   value)
  (cl:values))

;;; Load octet from memory.
(cl:defun load-octet (pointer)
  (cl:check-type pointer machine-word)
  (cl:let ((word (load-word (cl:logand pointer (cl:lognot +low-tag-mask+))))
	   (low-bits (cl:logand pointer +low-tag-mask+)))
    (cl:ldb (cl:byte 8 (cl:* (cl:if (cl:eq +endianness+ :little-endian)
				    low-bits
				    (cl:- 7 low-bits))
			     8))
	    word)))

;;; Store octed in memory.
(cl:defun store-octet (pointer value)
  (cl:check-type pointer machine-word)
  (cl:check-type value (cl:unsigned-byte 8))
  (cl:let ((word (load-word (cl:logand pointer (cl:lognot +low-tag-mask+))))
	   (low-bits (cl:logand pointer +low-tag-mask+)))
    (cl:setf (cl:ldb (cl:byte 8 (cl:* (cl:if (cl:eq +endianness+ :little-endian)
					     low-bits
					     (cl:- 7 low-bits))
				      8))
		     word)
	     value)
    (store-word (cl:logand pointer (cl:lognot +low-tag-mask+))
		word))
  (cl:values))
