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
  (loop for i from 0 to 63
        for address = (+ *bin-sizes-start* (* i 8))
        do (setf (sicl-gc-memory:memory-64 address)
                 (+ i 4)))
  (loop for i from 64 to 510
        for address = (+ *bin-sizes-start* (* i 8))
        for float-max-size = 67d0 then (* float-max-size *multiplier*)
        do (setf (sicl-gc-memory:memory-64 address)
                 (round float-max-size)))
  (setf (sicl-gc-memory:memory-64 (+ *bin-sizes-start* (* 511 8)))
        (expt 2 61)))
