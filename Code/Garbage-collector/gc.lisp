(cl:in-package #:sicl-gc)

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
;;; is as follows: we use a vector of half words (to guarantee that
;;; they will be fixnums).  The length of the vector is (/
;;; +nursery-size+ +word-size+), so to each bit in the bitvector
;;; corresponds a pair of words in the nursery.  Each word of the
;;; bitvector is a (/ +word-size+ 2) bit unsigned integer,
;;; representing liveness information for (/ +word-size+ 2)
;;; consecutive double words in the nursery.  The word at index i of
;;; the bitvector represents the objects at indexes between (*
;;; +word-size+ i)) and (1- (* +word-size+ (1+ i))) in the nursery.
;;; When the bit (ash 1 k) is set in word i, then the two consecutive
;;; words at indexes m and m+1 in the nursery are live, where m can be
;;; expressed as (* 2 (+ (* +word-size+ i) k)).
(defvar *nursery-live*)

;;; Slide the objects in the nursery according to the liveness bitvector
(defun slide-objects (nursery nursery-live)
  (let ((run-time (get-internal-run-time)))
    ;; Find the position of the first word in the vector representing
    ;; the liveness bitvector that has a `0' in it, meaning the
    ;; nursery has its first free position in the corresponding place.
    ;; There is always such a position, because we trigger a
    ;; collection before the heap is exhausted.
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
                         ;; We find a position containing a live object.
                         (setf (aref nursery to-pos) (aref nursery from-pos))
                         (setf (aref nursery (1+ to-pos)) (aref nursery (1+ from-pos)))
                         (incf to-pos 2))
                       (increment-from))))
          (incf *nursery-recovered-space* (- +nursery-size+ to-pos)))))
    (incf *nursery-slide-time* (- (get-internal-run-time) run-time))))
