(cl:in-package #:sicl-memory)

;;; Simulate a memory of 2^61 8-byte WORDS, corresponding to a size of
;;; 2^64 bytes.  We organize the memory into 8 levels, 7 levels of
;;; directories and 1 level of pages.  Each level corresponds to 8
;;; address bits so that a directory always has 256 entries.  A page
;;; stores the equivalent of 256 bytes, or 32 8-byte words.  
;;;
;;; Word accesses are always word aligned.  We allow for accesses to
;;; 4-byte words, 2-byte words, and individual bytes.  A 4-byte words
;;; is aligned on a 4-byte boundary, and a 2-byte word is aligned on a
;;; 2-byte boundary. 

;;; FIXME: get this from the general configuration.
(defconstant +unbound+ #xfffffffffffffffe)

;;; The page is 
(defun make-page ()
  (make-array 32 :initial-element +unbound+))

(defun make-directory ()
  (make-array 256 :initial-element nil))

(defparameter *memory* (make-directory))

(defun reset-memory ()
  (loop for i from 0 below 256
	do (setf (aref *memory* i) nil)))


(defun find-page-of-address (address)
  (let ((directory (loop with directory = *memory*
			 for shift from -56 to -16 by 8
			 for index = (logand (ash address shift) #xff)
			 do (when (null (aref directory index))
			      (setf (aref directory index) (make-directory)))
			    (setf directory (aref directory index))
			 finally (return directory)))
	(index (logand (ash address -8) #xff)))
    (when (null (aref directory index))
      (setf (aref directory index) (make-page)))
    (aref directory index)))

(defun split-address (address)
  (values (find-page-of-address address)
          (ldb (byte 5 3) address)
          (ldb (byte 3 0) address)))

(defun check-address-and-size (address size)
  (check-type size (member 8 16 32 64))
  (assert (zerop (mod address (ash size -3)))))

(defun load-unsigned (address size)
  (check-address-and-size address size)
  (multiple-value-bind (page page-index position)
      (split-address address)
    (ldb (byte size position) (aref page page-index))))

(defun load-signed (address size)
  (let ((unsigned (load-unsigned address size)))
    (if (>= unsigned (ash 1 (1- size)))
        (- unsigned (ash 1 size))
        unsigned)))

(defun store-unsigned (value address size)
  (check-address-and-size address size)
  (multiple-value-bind (page page-index position)
      (split-address address)
    (setf (ldb (byte size position) (aref page page-index))
          value)))

(defun store-signed (value address size)
  (let ((unsigned
          (if (minusp value)
              (+ value (ash 1 size))
              value)))
    (store-unsigned unsigned address size)))
