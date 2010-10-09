;;; Sliding garbage collector for the nursery.  We are assuming an
;;; invariant that says that the only references to an object in the
;;; nursery are from the roots or from other objects in the nursery.
;;; This means that there can be no reference from an older generation
;;; to an object in the nursery.  We maintain this invariant by using
;;; a write barrier.  Whenever an attempt is made to store a pointer
;;; to an object A in the nursery in an object B in an older
;;; generation, we move A and its transitive closure to an older
;;; generation.

;;; We trace the live objects and we obtain a bitvector (or
;;; equivalent) of information about what words in the nursery are
;;; live and what words are dead.  We want to compact the nursery by
;;; "sliding" the live objects towards lower indexes of the nursery.
;;; Since we are moving objects, any address that points to an object
;;; in the nursery that is going to be moved needs to be modified to
;;; reflect the motion.  The amount that an object is moved is
;;; determined by the total free space before it in the nursery. 

(defconstant +word-size+ 64)

;;; The size of the nursery in words.  This should be a multiple 
;;; of the word size.
(defconstant +nursery-size+ (* +word-size+ (expt 2 12)))

;;; In each thread, this variable gets bound to a vector that contains
;;; the youngest generation objects.  An object takes up at least two
;;; words in the nursery, and objects are aligned on a double-word
;;; boundary.
(defvar *nursery*)

;;; In each thread, this variable gets bound to a vector of words.
;;; The length of the vector is (/ +nursery-size+ +word-size 2).  Each
;;; word is an unsigned integer, but it represents a bitvector, that
;;; contains liveness information of the objects in the nursery.  The
;;; word at index i of the vector represents the objects at indexes
;;; between (* 2 +word-size+ i)) and (1- (* 2 +word-size+ (1+ i))).
;;; When the bit (ash 1 k) is set in word i, then the two consecutive
;;; words at indexes m and m+1 in the nursery are free, where m can be
;;; expressed as (* 2 (+ (* +word-size+ i) k)).  By doing it this way,
;;; we can use integer-length to find the most significant bit that is
;;; set in a word, and thus the corresponding free word with the
;;; highest index.
(defvar *nursery-live*)

;;; After the *nursery-live* bitvector has been created by tracing
;;; live objects, we use the nex-to-last word of every block of
;;; otherwise unused words to store the total number of unused words
;;; that precede the block, including the size of the block itself.
;;; The reason we use the next-to-last one is that it has an even
;;; address.  
(defun compute-sizes ()
  (let ((total-free-space 0)
	(nursery *nursery*)
	(nursery-live *nursery-live*)
	(length (length *nursery-live*)))
    ;; Check all transitions from 0 to 1, i.e. end of
    ;; free blocks not at the end of the nurserty. 
    (loop for i from (1+ (ash length -1)) below length
	  do (when (zerop (sbit nursery-live (1- i)))
	       (incf total-free-space 2)
	       (unless (zerop (sbit nursery-live i))
		 (setf (aref nursery (* 2 (1- i)))
		       total-free-space))))
    ;; Check whether the last word of the nursery is free. 
    (when (zerop (sbit nursery-live (1- length)))
      (setf (aref nursery (* 2 (1- length)))
	    (+ total-free-space)))))

(defun compute-cache (nursery-live cache)
  (let ((first (position-if-not #'zerop nursery-live)))
    (assert (not (null first)))
    (loop for i from first below (length nursery-live)
	  do (setf (aref cache i)
		   (if (zerop (aref nursery-live i))
		       (aref cache (1- i))
		       i)))))

(defun find-free-position (nursery-live cache position)
  (declare (type fixnum position))
  (multiple-value-bind (quotient remainder)
      (floor position +word-size+)
    (let* ((cache-entry (aref cache quotient)))
      (if (= cache-entry quotient)
	  (let ((len (integer-length (logand (aref nursery-live cache-entry)
					     (1- (ash 1 remainder))))))
	    (if (zerop len)
		(let ((cache-entry (aref cache (1- quotient))))
		  (+ (* cache-entry +word-size+)
		     (1- (integer-length (aref nursery-live cache-entry)))))
		(+ (* cache-entry +word-size+) (1- len))))
	  (+ (* cache-entry +word-size+)
	     (1- (integer-length (aref nursery-live cache-entry))))))))

;;; Given an even index into the nursery of a live object, find the
;;; amount of free space that precedes that live object.  We do this
;;; by searching for the free block in the nursery with the largest
;;; index smaller than that of the live object, and accessing the
;;; value that is stored in it. 
(defun free-space-preceding-index (index nursery nursery-live)
  ;; We first need to find the topmost pair of index bits from which
  ;; to start the search.
  (let ((i 2)
	(size (length nursery)))
    ;; we should be able to use arithmetic here. 
    (loop until (< (/ size 2) index)
	  do (setf size (/ size 2))
	     (setf i (* 2 i)))
    (format t "i now: ~s~%" i)
    ;; go down to the leaves
    (loop until (= size 4)
	  do (format t "size: ~s  i: ~s~%" size i)
	     (setf size (/ size 2))
	     (setf i (* 2 (if (zerop (sbit nursery-live (1+ i)))
			      (1+ i)
			      i))))
    (aref nursery (- i size))))
  
	 
	     

;;; In each thread, this variable gets bound to the address of element
;;; zero of the nursery. 
(defvar *nursery-address*)

