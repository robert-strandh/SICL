(cl:in-package #:sicl-utilities)

;;; The functions FLP2 and CLP2 round up or down to the next power of two,
;;; respectively.  The techniques are straight from "Hacker's Delight"
;;; Chapter 3-1, with some adaptions to support fixnums and bignums instead
;;; of 32bit unsigned integers.

(defun flp2 (n)
  "Round the unsigned integer N down to the next smaller multiple of two."
  (etypecase n
    (fixnum
     (let ((x n))
       (declare (type (and fixnum unsigned-byte) x))
       (setf x (logior x (ash x -1)))
       (setf x (logior x (ash x -2)))
       (setf x (logior x (ash x -4)))
       (setf x (logior x (ash x -8)))
       (setf x (logior x (ash x -16)))
       (setf x (logior x (ash x -32)))
       (- x (ash x -1))))
    (unsigned-byte
     (ash 1 (1- (integer-length n))))))

(deftype clp2-fixnum ()
  `(integer 0 ,(expt 2 (1- (integer-length most-positive-fixnum)))))

(defun clp2 (n)
  "Round the unsigned integer N up to the next larger multiple of two."
  (etypecase n
    (clp2-fixnum
     (when (zerop n)
       (return-from clp2 0))
     (let ((x (1- n)))
       (declare (type clp2-fixnum x))
       (setf x (logior x (ash x -1)))
       (setf x (logior x (ash x -2)))
       (setf x (logior x (ash x -4)))
       (setf x (logior x (ash x -8)))
       (setf x (logior x (ash x -16)))
       (setf x (logior x (ash x -32)))
       (1+ x)))
    (unsigned-byte
     (ash 1 (integer-length (1- n))))))
