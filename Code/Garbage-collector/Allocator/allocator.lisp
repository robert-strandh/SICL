(cl:in-package #:sicl-allocator)

(defun chunk-size (chunk)
  (assert (zerop (mod chunk 8)))
  (logand (sicl-gc-memory:memory-64 chunk) -1))

(defun (setf chunk-size) (new-size chunk)
  (assert (zerop (mod chunk 8)))
  (assert (zerop (mod new-size 8)))
  (setf (sicl-gc-memory:memory-64 chunk)
        (logior new-size
                (logand (sicl-gc-memory:memory-64 chunk) 1))))

(defun update-chunk-trailer-size (chunk)
  (let* ((size (chunk-size chunk))
         (last-word-address (+ chunk size -8)))
    (setf (sicl-gc-memory:memory-64 last-word-address) size)))

(defun chunk-prev-slot-address (chunk)
  (assert (zerop (mod chunk 8)))
  (+ chunk 8))

(defun chunk-next-slot-address (chunk)
  (assert (zerop (mod chunk 8)))
  (+ chunk 16))

(defun preceding-chunk-free-p (chunk)
  (assert (zerop (mod chunk 8)))
  (zerop (mod (sicl-gc-memory:memory-64 chunk) 2)))

(defun preceding-chunk-in-use-p (chunk)
  (assert (zerop (mod chunk 8)))
  (not (preceding-chunk-free-p chunk)))

(defun preceding-chunk-size (chunk)
  (assert (zerop (mod chunk 8)))
  (assert (preceding-chunk-free-p chunk))
  (sicl-gc-memory:memory-64 (- chunk 8)))
  
(defun preceding-chunk (chunk)
  (assert (zerop (mod chunk 8)))
  (assert (preceding-chunk-free-p chunk))
  (- chunk (preceding-chunk-size chunk)))

(defun following-chunk (chunk)
  (assert (zerop (mod chunk 8)))
  (+ chunk (chunk-size chunk)))

(defun chunk-free-p (chunk)
  (assert (zerop (mod chunk 8)))
  (preceding-chunk-free-p (following-chunk chunk)))

(defun chunk-linked-p (chunk)
  (assert (zerop (mod chunk 8)))
  (and (not (zerop (sicl-gc-memory:memory-64 (chunk-prev-slot-address chunk))))
       (not (zerop (sicl-gc-memory:memory-64 (chunk-next-slot-address chunk))))))

(defun unlink-chunk (chunk)
  (assert (zerop (mod chunk 8)))
  (assert (chunk-linked-p chunk))
  (let* ((pa (chunk-prev-slot-address chunk))
         (p (sicl-gc-memory:memory-64 pa))
         (na (chunk-next-slot-address chunk))
         (n (sicl-gc-memory:memory-64 na)))
    (setf (sicl-gc-memory:memory-64 p) n)
    (setf (sicl-gc-memory:memory-64 n) p)
    (setf (sicl-gc-memory:memory-64 pa) 0)
    (setf (sicl-gc-memory:memory-64 na) 0)))

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
