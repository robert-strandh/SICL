(cl:in-package #:sicl-allocator)

(defconstant +prev-slot-offset+ (* 1 8))

(defconstant +next-slot-offset+ (* 2 8))

(defun valid-chunk-alignment-p (chunk)
  (zerop (mod chunk 8)))

(defun chunk-size (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (logand (sicl-gc-memory:memory-64 chunk) -1))

(defun (setf chunk-size) (new-size chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (zerop (mod new-size 8)))
  (setf (sicl-gc-memory:memory-64 chunk)
        (logior new-size
                (logand (sicl-gc-memory:memory-64 chunk) 1))))

(defun update-chunk-trailer-size (chunk)
  (let* ((size (chunk-size chunk))
         (last-word-address (+ chunk size -8)))
    (setf (sicl-gc-memory:memory-64 last-word-address) size)))

(defun chunk-prev-slot-address (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (+ chunk +prev-slot-offset+))

(defun chunk-next-slot-address (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (+ chunk +next-slot-offset+))

(defun preceding-chunk-free-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (zerop (mod (sicl-gc-memory:memory-64 chunk) 2)))

(defun preceding-chunk-in-use-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (not (preceding-chunk-free-p chunk)))

(defun preceding-chunk-size (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (preceding-chunk-free-p chunk))
  (sicl-gc-memory:memory-64 (- chunk 8)))
  
(defun preceding-chunk (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (preceding-chunk-free-p chunk))
  (- chunk (preceding-chunk-size chunk)))

(defun following-chunk (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (+ chunk (chunk-size chunk)))

(defun chunk-free-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (preceding-chunk-free-p (following-chunk chunk)))

;;; When a chunk is not linked, for reasons of debugging, we set the
;;; PREV and NEXT slots to 1.  Since this number is not a multiple of
;;; 8, it is an illegal pointer.
(defun chunk-linked-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (and (not (= (sicl-gc-memory:memory-64 (chunk-prev-slot-address chunk)) 1))
       (not (= (sicl-gc-memory:memory-64 (chunk-next-slot-address chunk)) 1))))

(defun unlink-chunk (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (chunk-linked-p chunk))
  (let* ((pa (chunk-prev-slot-address chunk))
         (p (sicl-gc-memory:memory-64 pa))
         (na (chunk-next-slot-address chunk))
         (n (sicl-gc-memory:memory-64 na)))
    (setf (sicl-gc-memory:memory-64 p) n)
    (setf (sicl-gc-memory:memory-64 n) p)
    (setf (sicl-gc-memory:memory-64 pa) 1)
    (setf (sicl-gc-memory:memory-64 na) 1)))

;;; Link CHUNK into the doubly linked list of a bin.  PNA is the
;;; address of the NEXT slot of the chunk that will become the
;;; previous chunk of CHUNK in the bin, or the address of the START
;;; sentinel if there is no previous chunk.  NPA is the address of the
;;; PREVIOUS slot of the chunk that will become the next chunk of
;;; CHUNK in the bin, or the address of the END sentinel if there is
;;; no next chunk.
(defun link-chunk-between (chunk pna npa)
  (setf (sicl-gc-memory:memory-64 (chunk-prev-slot-address chunk)) pna)
  (setf (sicl-gc-memory:memory-64 (chunk-next-slot-address chunk)) npa)
  (setf (sicl-gc-memory:memory-64 pna) (chunk-prev-slot-address chunk))
  (setf (sicl-gc-memory:memory-64 npa) (chunk-next-slot-address chunk)))

(defun coalesce-two-chunks (chunk1 chunk2)
  (assert (chunk-free-p chunk1))
  (assert (chunk-free-p chunk2))
  (incf (chunk-size chunk1) (chunk-size chunk2))
  (update-chunk-trailer-size chunk1)
  chunk1)

(defun coalesce-three-chunks (chunk1 chunk2 chunk3)
  (assert (chunk-free-p chunk1))
  (assert (chunk-free-p chunk2))
  (assert (chunk-free-p chunk3))
  (incf (chunk-size chunk1) (+ (chunk-size chunk2) (chunk-size chunk3)))
  (update-chunk-trailer-size chunk1)
  chunk1)
