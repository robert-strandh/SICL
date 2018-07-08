(cl:in-package #:sicl-allocator)

(defun chunk-size (chunk)
  (assert (zerop (mod chunk 8)))
  (logand (sicl-gc-memory:memory-64 chunk) -1))

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

