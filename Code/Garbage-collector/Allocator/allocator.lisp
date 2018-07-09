(cl:in-package #:sicl-allocator)

(defparameter *number-of-bins* 512)

;;; The address in memory where the vector of start sentinels starts.
(defparameter *start-sentinels-start* 0)

;;; The address in memory where the vector of end sentinels starts.
(defparameter *end-sentinels-start*
  (+ *start-sentinels-start* (* *number-of-bins* 8)))

;;; The address in memory where the vector of bin sizes starts.
(defparameter *bin-sizes-start*
  (+ *end-sentinels-start* (* *number-of-bins* 8)))


;;; Bin i, where 0 <= i <= 63 contains i+4 words.  For 64 <= i <= 511,
;;; we express the max size of a chunk as a fixed multiplier of the
;;; max size of a chunk in bin i-1.  The additional constraint is that
;;; the max size of a chunk in bin 511 should be 2^61 words.  We get
;;; the formula:
;;;
;;;  67*m^(511-63) = 2^61
;;;
;;; or
;;;
;;;  p = e^(ln (2^61 / 67) / (511 - 63))
;;;
;;; which gives a p around 1.089.

(defparameter *multiplier*
  (exp (/ (log (/ (expt 2d0 61d0) 67d0)) (- 511d0 63d0))))

(defun init-bin-sizes ()
  (loop for index from 0 to 63
        for address = (+ *bin-sizes-start* (* index 8))
        do (setf (sicl-gc-memory:memory-64 address)
                 (* (+ index 4) 8)))
  (loop for index from 64 to 510
        for address = (+ *bin-sizes-start* (* index 8))
        for float-max-size = (* 67d0 8d0 *multiplier*)
          then (* float-max-size *multiplier*)
        do (setf (sicl-gc-memory:memory-64 address)
                 (round float-max-size)))
  (setf (sicl-gc-memory:memory-64 (+ *bin-sizes-start* (* 511 8)))
        (expt 2 64)))

(defun init-sentinels ()
  (loop for index from 0 below 512
        for sentinel-offset = (* index 8)
        for start-sentinel-address = (+ *start-sentinels-start* sentinel-offset)
        for end-sentinel-address = (+ *end-sentinels-start* sentinel-offset)
        do (setf (sicl-gc-memory:memory-64 start-sentinel-address)
                 end-sentinel-address)
           (setf (sicl-gc-memory:memory-64 end-sentinel-address)
                 start-sentinel-address)))

;;; Given a chunk size in bytes, find the bin offset in bytes from the
;;; beginning of the bin vector that is the smallest one big enough to
;;; hold a chunk of size SIZE.
(defun find-ideal-bin-offset (size)
  (if (<= size (* 67 8))
      ;; The size fits exactly in one of the first 64 bins.  So we
      ;; subtract 4 words or 4*8 bytes from the size to get to the
      ;; correct bin offset.
      (- size (* 4 8))
      ;; The size is too big for our exact bins.  So we do a binary
      ;; search for the right bin.  The convention here is that the
      ;; right address is the one immediately after the last element
      ;; to be considered.
      (loop with left-address = (+ *bin-sizes-start* (* 64 8))
            with right-address = (+ *bin-sizes-start* (* 512 8))
            until (= right-address (+ left-address 8))
            do (let* ((middle-address (ash (+ left-address right-address) -1))
                      ;; We really want the entry immediately to the
                      ;; left of the middle so as to decide whether it
                      ;; is too small or not.
                      (address (- middle-address 8))
                      (value (sicl-gc-memory:memory-64 address)))
                 (if (< value size)
                     ;; The bin to the left of the middle is too
                     ;; small, so we exclude it by setting the left
                     ;; address to the middle one.
                     (setf left-address middle-address)
                     ;; The bin to the left of the middle is big
                     ;; enough, so we exclude the middle and
                     ;; everything to the right of it by setting the
                     ;; right address to the middle one.
                     (setf right-address middle-address)))
            finally (return (- left-address *bin-sizes-start*)))))

(defun link-chunk-by-address (chunk start-sentinel-address end-sentinel-address)
  (loop for pna = start-sentinel-address then (+ npa 8)
        for npa = (sicl-gc-memory:memory-64 start-sentinel-address)
          then (sicl-gc-memory:memory-64 (+ npa 8))
        until (or (= npa end-sentinel-address)
                  (> npa chunk))
        finally (link-chunk-between chunk pna npa)))

(defun link-chunk-by-size-and-address
    (chunk start-sentinel-address end-sentinel-address)
  (loop with size = (chunk-size chunk)
        for pna = start-sentinel-address then (+ npa 8)
        for npa = (sicl-gc-memory:memory-64 start-sentinel-address)
          then (sicl-gc-memory:memory-64 (+ npa 8))
        until (or (= npa end-sentinel-address)
                  (> (chunk-size (- npa 8)) size)
                  (and (= (chunk-size (- npa 8)) size)
                       (> npa chunk)))
        finally (link-chunk-between chunk pna npa)))

(defun link-chunk (chunk)
  (let* ((size (chunk-size chunk))
         (offset (find-ideal-bin-offset size))
         (start-sentinel-address (+ *start-sentinels-start* offset))
         (end-sentinel-address (+ *end-sentinels-start* offset)))
    (if (<= size (* 67 8))
        (link-chunk-by-address
         chunk start-sentinel-address end-sentinel-address)
        (link-chunk-by-size-and-address
         chunk start-sentinel-address end-sentinel-address))))

(defun free (chunk)
  (assert (not (chunk-free-p chunk)))
  (let ((following-chunk (following-chunk chunk)))
    (if (preceding-chunk-free-p chunk)
        (let ((preceding-chunk (preceding-chunk chunk)))
          (if (chunk-free-p following-chunk)
              (progn (unlink-chunk preceding-chunk)
                     (unlink-chunk following-chunk)
                     (coalesce-three-chunks preceding-chunk
                                            chunk
                                            following-chunk)
                     (link-chunk preceding-chunk))
              (progn (unlink-chunk preceding-chunk)
                     (coalesce-two-chunks preceding-chunk chunk)
                     (link-chunk preceding-chunk))))
        (progn (unlink-chunk following-chunk)
               (coalesce-two-chunks chunk following-chunk)
               (link-chunk chunk)))))

;;; A bin is empty if and only if the start sentinel points to the
;;; slot of the end sentinel.
(defun bin-empty-p (bin-offset)
  (let ((start-sentinel-address (+ *start-sentinels-start* bin-offset))
        (end-sentinel-address (+ *end-sentinels-start* bin-offset)))
    (= (sicl-gc-memory:memory-64 start-sentinel-address) end-sentinel-address)))

;;; Given the offset of an initial bin, find the first one that is not
;;; empty.
(defun find-first-non-empty-bin (first-bin-offset)
  (loop for bin-offset = first-bin-offset then (+ bin-offset 8)
        while (and (< bin-offset (* 512 8)) (not (bin-empty-p bin-offset)))
        finally (if (= bin-offset (* 512 8))
                    (error "memory exhausted")
                    (return bin-offset))))

;;; Given a bin offset and a chunk size in bytes, find the first chunk
;;; in the bin with that offset that is at least as large as that
;;; size.  If no such chunk exists, return NIL.
(defun find-first-big-enough-chunk (bin-offset size)
  (let ((start-sentinel-address (+ *start-sentinels-start* bin-offset))
        (end-sentinel-address (+ *end-sentinels-start* bin-offset)))
    (loop for chunkprev = (sicl-gc-memory:memory-64 start-sentinel-address)
            then (sicl-gc-memory:memory-64 chunkprev)
          until (or (= chunkprev end-sentinel-address)
                    (<= size (chunk-size (- chunkprev 8))))
          finally (return (if (= chunkprev end-sentinel-address)
                              nil
                              (- chunkprev 8))))))

;;; Given a size in bytes, find the best-fit chunk.
(defun find-chunk (size)
  (let ((bin-offset (find-ideal-bin-offset size)))
    (loop for offset = (find-first-non-empty-bin bin-offset)
            then (find-first-non-empty-bin (+ offset 8))
          do (if (<= offset (* 63 8))
                 ;; We have a non-empty bin with all chunks being the
                 ;; same size, so we just return the first chunk.
                 (return (- (sicl-gc-memory:memory-64
                             (+ *start-sentinels-start* offset))
                            8))
                 ;; We have a non-empty bin that may or may not contain
                 ;; a big-enough chunk.
                 (let ((chunk (find-first-big-enough-chunk offset size)))
                   (unless (null chunk)
                     ;; We found a candidate chunk.  Return it.
                     (return chunk)))))))

;;; Given a size in bytes, allocate and return a best-fit chunk.
(defun allocate-chunk (size)
  (let ((candidate (find-chunk size)))
    (unlink-chunk candidate)
    (if (< (chunk-size candidate) (+ size (* 4 8)))
        ;; Return the entire thing, since if we were to split of the
        ;; right size, we would get a chunk that is smaller than the
        ;; smallest acceptable size.
        candidate
        ;; Otherwise, we split the chunk into one chunk of the exact
        ;; size we want, and one chunk containing the remaining bytes.
        (let* ((residue-size (- size (chunk-size candidate)))
               (residue-chunk (+ candidate residue-size)))
          (setf (chunk-size candidate) size)
          (update-chunk-trailer-size candidate)
          (setf (chunk-size residue-chunk) residue-size)
          (update-chunk-trailer-size residue-chunk)
          ;; Before linking the residue chunk, we must make sure that the
          ;; PREV and NEXT links contain 1, indicating that the chunk is unlinked.
          (setf (sicl-gc-memory:memory-64 (+ residue-chunk +prev-slot-offset+))
                1)
          (setf (sicl-gc-memory:memory-64 (+ residue-chunk +next-slot-offset+))
                1)
          (link-chunk residue-chunk)
          candidate))))
