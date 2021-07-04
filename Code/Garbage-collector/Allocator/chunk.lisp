(cl:in-package #:sicl-allocator)

(defconstant +prev-slot-offset+ (* 1 8))

(defconstant +next-slot-offset+ (* 2 8))

(defun valid-chunk-alignment-p (chunk)
  (zerop (mod chunk 8)))

(defun chunk-size (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (logand (sicl-memory:memory chunk 64) -4))

(defun (setf chunk-size) (new-size chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (zerop (mod new-size 8)))
  (setf (sicl-memory:memory chunk 64)
        (logior new-size
                (logand (sicl-memory:memory chunk 64) 3))))

(defun update-chunk-trailer-size (chunk)
  (let* ((size (chunk-size chunk))
         (last-word-address (+ chunk size -8)))
    (setf (sicl-memory:memory last-word-address 64) size)))

(defun chunk-prev-slot-address (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (+ chunk +prev-slot-offset+))

(defun chunk-next-slot-address (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (+ chunk +next-slot-offset+))

(defun preceding-chunk-free-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (zerop (mod (sicl-memory:memory chunk 64) 2)))

(defun preceding-chunk-in-use-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (not (preceding-chunk-free-p chunk)))

(defun preceding-chunk-size (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (preceding-chunk-free-p chunk))
  (sicl-memory:memory (- chunk 8) 64))
  
(defun preceding-chunk (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (preceding-chunk-free-p chunk))
  (- chunk (preceding-chunk-size chunk)))

(defun following-chunk (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (+ chunk (chunk-size chunk)))

(defun chunk-free-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (zerop (logand (sicl-memory:memory chunk 64) 2)))

(defun (setf chunk-free-p) (free-p chunk)
  (assert (valid-chunk-alignment-p chunk))
  (let ((following-chunk (following-chunk chunk)))
    (if free-p
        (progn (setf (sicl-memory:memory chunk 64)
                     (logand (sicl-memory:memory chunk 64) -3))
               (unless (= following-chunk *heap-end*)
                 (setf (sicl-memory:memory following-chunk 64)
                       (logand (sicl-memory:memory following-chunk 64) -2))))
        (progn (setf (sicl-memory:memory chunk 64)
                     (logior (sicl-memory:memory chunk 64) 2))
               (unless (= following-chunk *heap-end*)
                 (setf (sicl-memory:memory following-chunk 64)
                       (logior (sicl-memory:memory following-chunk 64) 1)))))))

;;; When a chunk is not linked, for reasons of debugging, we set the
;;; PREV and NEXT slots to 1.  Since this number is not a multiple of
;;; 8, it is an illegal pointer.
(defun chunk-linked-p (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (and (not (= (sicl-memory:memory (chunk-prev-slot-address chunk) 64) 1))
       (not (= (sicl-memory:memory (chunk-next-slot-address chunk) 64) 1))))

(defun mark-chunk-as-unlinked (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (chunk-linked-p chunk))
  (let* ((pa (chunk-prev-slot-address chunk))
         (na (chunk-next-slot-address chunk)))
    (setf (sicl-memory:memory pa 64) 1)
    (setf (sicl-memory:memory na 64) 1)))

(defun unlink-chunk (chunk)
  (assert (valid-chunk-alignment-p chunk))
  (assert (chunk-linked-p chunk))
  (let* ((pa (chunk-prev-slot-address chunk))
         (p (sicl-memory:memory pa 64))
         (na (chunk-next-slot-address chunk))
         (n (sicl-memory:memory na 64)))
    (setf (sicl-memory:memory p 64) n)
    (setf (sicl-memory:memory n 64) p)
    (mark-chunk-as-unlinked chunk)))

;;; Link CHUNK into the doubly linked list of a bin.  PNA is the
;;; address of the NEXT slot of the chunk that will become the
;;; previous chunk of CHUNK in the bin, or the address of the START
;;; sentinel if there is no previous chunk.  NPA is the address of the
;;; PREVIOUS slot of the chunk that will become the next chunk of
;;; CHUNK in the bin, or the address of the END sentinel if there is
;;; no next chunk.
(defun link-chunk-between (chunk pna npa)
  (setf (sicl-memory:memory (chunk-prev-slot-address chunk) 64) pna)
  (setf (sicl-memory:memory (chunk-next-slot-address chunk) 64) npa)
  (setf (sicl-memory:memory pna 64) (chunk-prev-slot-address chunk))
  (setf (sicl-memory:memory npa 64) (chunk-next-slot-address chunk)))

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
