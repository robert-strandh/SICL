(cl:in-package #:sicl-gc-memory)

(defparameter *memory* (make-array (expt 2 30) :element-type '(unsigned-byte 8)))

(defun end-memory ()
  (length *memory*))

(defun memory-8 (address)
  (aref *memory* address))

(defun (setf memory-8) (contents address)
  (check-type contents (unsigned-byte 8))
  (setf (aref *memory* address) contents)
  contents)

(defun memory-16 (address)
  (assert (zerop (mod address 2)))
  (logior(memory-8 address)
         (ash (memory-8 (+ address 1)) 8)))

(defun (setf memory-16) (contents address)
  (check-type contents (unsigned-byte 16))
  (assert (zerop (mod address 2)))
  (setf (memory-8 address) (ldb (byte 8 0) contents)
        (memory-8 (+ address 1)) (ldb (byte 8 8) contents))
  contents)

(defun memory-32 (address)
  (assert (zerop (mod address 4)))
  (logior(memory-16 address)
         (ash (memory-16 (+ address 2)) 16)))

(defun (setf memory-32) (contents address)
  (check-type contents (unsigned-byte 32))
  (assert (zerop (mod address 4)))
  (setf (memory-16 address) (ldb (byte 16 0) contents)
        (memory-16 (+ address 2)) (ldb (byte 16 16) contents))
  contents)

(defun memory-64 (address)
  (assert (zerop (mod address 8)))
  (logior(memory-32 address)
         (ash (memory-32 (+ address 4)) 32)))

(defun (setf memory-64) (contents address)
  (check-type contents (unsigned-byte 64))
  (assert (zerop (mod address 8)))
  (setf (memory-32 address) (ldb (byte 32 0) contents)
        (memory-32 (+ address 4)) (ldb (byte 32 32) contents))
  contents)
