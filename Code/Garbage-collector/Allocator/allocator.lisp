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
