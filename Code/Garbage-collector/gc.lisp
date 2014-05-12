(in-package #:sicl-gc)

;;; Sliding garbage collector for the nursery.

;;; We assume an invariant that says that the only references to an
;;; object in the nursery are from the roots or from other objects in
;;; the nursery.  This means that there can be no reference from a the
;;; shared heap to an object in the nursery.  We maintain this
;;; invariant by using a write barrier.  Whenever an attempt is made
;;; to store a pointer to an object A in the nursery in an object B in
;;; the shared heap, we move A and its transitive closure to an older
;;; generation.

;;; We trace the live objects and we obtain a bitvector (though not
;;; represented as a Common Lisp bitvector) of information about what
;;; words in the nursery are live and what words are dead.  We want to
;;; compact the nursery by "sliding" the live objects towards lower
;;; indexes of the nursery.  Since we are moving objects, any address
;;; that points to an object in the nursery that is going to be moved
;;; needs to be modified to reflect the motion.  The amount that an
;;; object is moved is determined by the total free space before it in
;;; the nursery.

(defconstant +word-size+ 64)

;;; The size of the nursery in words.  This should be a multiple of
;;; the word size and smaller than the size of the processor cache.
;;; We currently define it to be 2^19 64-bit words, because this gives
;;; a size of 2^22 bytes = 4MB which corresponds to a typical cache
;;; size.
(defconstant +nursery-size+ (* +word-size+ (expt 2 19)))

;;; In each thread, this variable gets bound to a vector that contains
;;; the nursery.  The vector contains unsigned words, so its type is
;;; (array (integer 0 (#.+word-size+)) (#.+nursery-size)).
(defvar *nursery*)

;;; In each thread, this variable gets bound to a non-negative fixnum
;;; indicating the address of the beginning of the nursery.
(defvar *nursery-address*)

;;; In each thread, this variable gets bound to a bitvector (though
;;; not represented as a common Lisp bitvector).  The representation
;;; is as follows: we use a vector of words.  The length of the vector
;;; is (/ +nursery-size+ +word-size+ 2), so to each bit in the
;;; bitvector corresponds a pair of words in the nursery.  Each word
;;; of the bitvector is a +word-size+ unsigned integer, representing
;;; liveness information for +word-size+ consecutive double words in
;;; the nursery.  The word at index i of the bitvector vector
;;; represents the objects at indexes between (* 2 +word-size+ i)) and
;;; (1- (* 2 +word-size+ (1+ i))) in the nursery.  When the bit (ash 1
;;; k) is set in word i, then the two consecutive words at indexes m
;;; and m+1 in the nursery are live, where m can be expressed as (* 2
;;; (+ (* +word-size+ i) k)).
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

;;; We compute a cache for the liveness bitvector.  The cache has the
;;; same length as the vector representing the liveness bitvector.  At
;;; index i of the cache is stored the largest k <= i such that the
;;; vector representing the liveness bitvector has at least one `0' at
;;; index k.  When there is no such k, we don't care what is stored in
;;; index i, because it will never be used. 
(defun compute-cache (nursery-live cache)
  (let ((first (position-if-not #'zerop nursery-live))
	(run-time (get-internal-run-time)))
    (assert (not (null first)))
    (loop for i from first below (length nursery-live)
	  do (setf (aref cache i)
		   (if (zerop (aref nursery-live i))
		       (aref cache (1- i))
		       i)))
    (incf *nursery-cache-build-time* (- (get-internal-run-time) run-time))))

;;; Find the largest position in the liveness bitvector less than the 
;;; position given that contains a `1'. 
(defun find-free-position (nursery-live cache position)
  (declare (type fixnum position))
  (multiple-value-bind (quotient remainder)
      (floor position +word-size+)
    (let* ((cache-entry (aref cache quotient)))
      (if (= cache-entry quotient)
	  ;; The position we are looking for might be at index
	  ;; cache-entry. 
	  (let ((len (integer-length (logand (aref nursery-live cache-entry)
					     (1- (ash 1 remainder))))))
	    (if (zerop len)
		;; False alarm.  The position we are looking for
		;; is not at index cache-entry.  
		(let ((cache-entry (aref cache (1- quotient))))
		  (+ (* cache-entry +word-size+)
		     (1- (integer-length (aref nursery-live cache-entry)))))
		;; Found the position we are looking for at index
		;;; cache-entry. 
		(+ (* cache-entry +word-size+) (1- len))))
	  ;; The position we are looking for is definitely not at index
	  ;; cache-entry.  
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

;;; Slide the objects in the nursery according to the liveness bitvector
(defun slide-objects (nursery nursery-live)
  (let ((run-time (get-internal-run-time)))
    ;; Find the position of the first word in the vector representing the 
    ;; liveness bitvector that has a `1' in it, meaning the nursery has
    ;; its first free position in the corresponding place.
    (let* ((wordpos (position-if-not #'zerop nursery-live))
	   (word (aref nursery-live wordpos)))
      ;; Find the position of the least-significant `1' in the word
      ;; containing at least one `1'.
      (let ((bitpos (loop for i from 0 below +word-size+
			  when (logbitp i word)
			    return i)))
	(let ((to-pos (* (+ (* wordpos +word-size+) bitpos) 2)))
	  ;; Initialize the `from' position to be the same as the `to' position.
	  (let ((from-wordpos wordpos)
		(from-word word)
		(from-bitpos bitpos)
		(from-pos to-pos))
	    (flet ((increment-from ()
		     (incf from-pos 2)
		     (incf from-bitpos)
		     (when (= from-bitpos +word-size+)
		       (incf from-wordpos)
		       (setf from-bitpos 0)
		       (setf from-word (aref nursery-live from-wordpos)))))
	      (loop until (> to-pos +nursery-size+)
		    do (when (not (logbitp from-bitpos from-word))
			 ;; we find a position containing a live object
			 (setf (aref nursery to-pos) (aref nursery from-pos))
			 (setf (aref nursery (1+ to-pos)) (aref nursery (1+ from-pos)))
			 (incf to-pos 2))
		       (increment-from))))
	  (incf *nursery-recovered-space* (- +nursery-size+ to-pos)))))
    (incf *nursery-slide-time* (- (get-internal-run-time) run-time))))
