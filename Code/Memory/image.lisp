(cl:in-package #:sicl-memory)

;;; Simulate a memory of 2^61 8-byte WORDS, corresponding to a size of
;;; 2^64 bytes.  We organize the memory into 8 levels, 7 levels of
;;; directories and 1 level of pages.  Each level corresponds to 8
;;; address bits so that a directory always has 256 entries.  A page
;;; stores the equivalent of 256 bytes, or 32 8-byte words.  
;;;
;;; The reason we make the memory contain words instead of bytes is
;;; that we want to "cheat" in that we temporarily want to store host
;;; objects as well as target objects in the memory.  When a word of
;;; memory contains an integer, then it is always a non-negative
;;; integer, and its value is always between 0 and 2^64 - 1.  Such a
;;; word of memory always denotes a target object.  When a host object
;;; is stored, then it is never an integer.
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

(defun load-byte (address)
  (let((page (find-page-of-address address))
       (index (ldb (byte 5 3) address)))
    (ldb (byte 8 (* 8 (ldb (byte 3 0) address))) (aref page index))))

(defun store-byte (value address)
  (check-type value (unsigned-byte 8))
  (let* ((page (find-page-of-address address))
	 (index (ldb (byte 5 3) address)))
    (setf (ldb (byte 8 (* 8 (ldb (byte 3 0) address))) (aref page index))
	  value)))

(defun load-8-byte-word (address)
  (assert (zerop (ldb (byte 3 0) address)))
  (let ((page (find-page-of-address address))
	(index (ldb (byte 5 3) address)))
    (aref page index)))
  
(defun store-8-byte-word (value address)
  (assert (zerop (ldb (byte 3 0) address)))
  (when (integerp value)
    (check-type value (mod #.(expt 2 64))))
  (let ((page (find-page-of-address address))
	(index (ldb (byte 5 3) address)))
    (setf (aref page index) value)))

(defun load-4-byte-word (address)
  (assert (zerop (ldb (byte 2 0) address)))
  (let((page (find-page-of-address address))
       (index (ldb (byte 5 3) address)))
    (ldb (byte 32 (* 32 (ldb (byte 1 2) address))) (aref page index))))

(defun store-4-byte-word (value address)
  (assert (zerop (ldb (byte 2 0) address)))
  (check-type value (mod #.(expt 2 32)))
  (let((page (find-page-of-address address))
       (index (ldb (byte 5 3) address)))
    (setf (ldb (byte 32 (* 32 (ldb (byte 1 2) address))) (aref page index))
	  value)))

(defun load-2-byte-word (address)
  (assert (zerop (ldb (byte 1 0) address)))
  (let((page (find-page-of-address address))
       (index (ldb (byte 5 3) address)))
    (ldb (byte 16 (* 16 (ldb (byte 2 1) address))) (aref page index))))

(defun store-2-byte-word (value address)
  (assert (zerop (ldb (byte 1 0) address)))
  (check-type value (mod #.(expt 2 16)))
  (let((page (find-page-of-address address))
       (index (ldb (byte 5 3) address)))
    (setf (ldb (byte 16 (* 16 (ldb (byte 2 1) address))) (aref page index))
	  value)))
