(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

;;; We assume that the bag has been checked so that it is known to
;;; be a proper list of characters.
(defun character-in-list-bag-p (character bag)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop for c in bag
	when (char= character c)
	  return t))

(declaim (inline character-in-list-bag-p))

(defun character-in-simple-string-bag-p (character bag)
  (loop for i from 0 below (length bag)
	when (char= character (schar bag i))
	  return t))

(declaim (inline character-in-simple-string-bag-p))

(defun character-in-general-string-bag-p (character bag)
  (loop for i from 0 below (length bag)
	when (char= character (char bag i))
	  return t))

(declaim (inline character-in-general-string-bag-p))

(defun character-in-general-vector-bag-p (character bag)
  (loop for i from 0 below (length bag)
	when (char= character (aref bag i))
	  return t))

(declaim (inline character-in-general-vector-bag-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-LEFT-TRIM.

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; list, and a string represented as a simple string.
(defun string-left-trim-list-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-list-bag-p (schar string 0) character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-list-bag-p (schar string i) character-bag)
		  return (extract-interval-simple string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; list, and a string represented as a general string.
(defun string-left-trim-list-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-list-bag-p (char string 0) character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-list-bag-p (char string i) character-bag)
		  return (extract-interval-simple string i length)
		finally (return ""))))))

(defun string-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (first (position-if-not #'in-bag-p string)))
      (if (null first)
	  string
	  (let ((last (position-if-not #'in-bag-p string :from-end t)))
	    (subseq string first (1+ last)))))))

(defun string-left-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (first (position-if-not #'in-bag-p string)))
      (if (null first)
	  string
	  (subseq string first)))))

(defun string-right-trim (character-bag string-designator)
  (flet ((in-bag-p (char) (find char character-bag)))
    (let* ((string (string string-designator))
	   (last (position-if-not #'in-bag-p string :from-end t)))
      (if (null last)
	  string
	  (subseq string 0 (1+ last))))))
