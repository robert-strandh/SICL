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

(defun link-chunk-between (chunk pa na)
  (setf (sicl-gc-memory:memory-64 (chunk-prev-slot-address chunk)) pa)
  (setf (sicl-gc-memory:memory-64 (chunk-next-slot-address chunk)) na)
  (setf (sicl-gc-memory:memory-64 pa) (chunk-prev-slot-address chunk))
  (setf (sicl-gc-memory:memory-64 na) (chunk-next-slot-address chunk)))
