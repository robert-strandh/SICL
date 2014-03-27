;;;; Copyright (c) 2014
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the string module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file string.text for a description of the module.

(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

;;; Check that a list is a proper list of characters.  It has already been
;;; checked that the bag is a list.
(defun verify-list-bag (bag)
  (unless (null bag)
    (loop with slow = bag
	  with fast = bag
	  while (consp fast)
	  unless (characterp (car fast))
	    do (error 'bag-contains-non-character
		      :datum (car fast)
		      :expected-type 'character)
	  do (setf fast (cdr fast))
	  while (consp fast)
	  until (eq slow fast)
	  unless (characterp (car fast))
	    do (error 'bag-contains-non-character
		      :datum (car fast)
		      :expected-type 'character)
	  do (setf fast (cdr fast))
	     (setf slow (cdr slow))
	  finally (cond ((eq slow fast)
			 (error 'bag-is-circular-list
				:datum bag
				:expected-type 'proper-list))
			((and (atom fast) (not (null fast)))
			 (error 'bag-is-dotted-list
				:datum bag
				:expected-type 'proper-list))
			(t nil)))))

;;; Check that a vector contains only characters.
(defun verify-vector-bag (bag)
  (loop for element across bag
	unless (characterp element)
	  do (error 'bag-contains-non-character
		    :datum element
		    :expected-type 'character)))

;;; We assume that the bag has been checked so that it is known to
;;; be a proper list of characters.
(defun character-in-list-bag-p (character bag)
  (declare (type character character)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (loop for c in bag
	when (char= character c)
	  return t))

(declaim (inline character-in-list-bag-p))

(defun character-in-simple-string-bag-p (character bag)
  (declare (type character character)
	   (type simple-string bag)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (loop for i from 0 below (length bag)
	when (char= character (schar bag i))
	  return t))

(declaim (inline character-in-simple-string-bag-p))

(defun character-in-general-string-bag-p (character bag)
  (declare (type character character)
	   (type string bag)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (loop for i from 0 below (length bag)
	when (char= character (char bag i))
	  return t))

(declaim (inline character-in-general-string-bag-p))

(defun character-in-simple-vector-bag-p (character bag)
  (declare (type character character)
	   (type simple-vector bag)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (loop for i from 0 below (length bag)
	when (char= character (svref bag i))
	  return t))

(declaim (inline character-in-simple-vector-bag-p))

(defun character-in-general-vector-bag-p (character bag)
  (declare (type character character)
	   (type vector bag)
	   (optimize (speed 3) (safety 0) (debug 0)))
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
	    (not (character-in-list-bag-p
		  (schar string 0)
		  character-bag)))
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
	    (not (character-in-list-bag-p
		  (char string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-list-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; simple-string, and a string represented as a simple string.
(defun string-left-trim-simple-string-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-string-bag-p
		  (schar string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-simple-string-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; simple-string, and a string represented as a general string.
(defun string-left-trim-simple-string-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-string-bag-p
		  (char string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-simple-string-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; general-string, and a string represented as a simple string.
(defun string-left-trim-general-string-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-string-bag-p
		  (schar string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-general-string-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; general-string, and a string represented as a general string.
(defun string-left-trim-general-string-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-string-bag-p
		  (char string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-general-string-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; simple vector, and a string represented as a simple string.
(defun string-left-trim-simple-vector-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-vector-bag-p
		  (schar string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-simple-vector-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; simple vector, and a string represented as a general string.
(defun string-left-trim-simple-vector-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-vector-bag-p
		  (char string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-simple-vector-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; general vector, and a string represented as a simple string.
(defun string-left-trim-general-vector-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-vector-bag-p
		  (schar string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-general-vector-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string i length)
		finally (return ""))))))

;;; A version of STRING-LEFT-TRIM for a character bag represented as a
;;; general vector, and a string represented as a general string.
(defun string-left-trim-general-vector-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-vector-bag-p
		  (char string 0)
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum from 0 below length
		unless (character-in-general-vector-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string i length)
		finally (return ""))))))

(defun string-left-trim (character-bag string-designator)
  (let ((string (string string-designator))
	(bag character-bag))
    (etypecase bag
      (list
       (verify-list-bag bag)
       (if (simple-string-p string)
	   (string-left-trim-list-simple-string bag string)
	   (string-left-trim-list-general-string bag string)))
      (simple-string
       (if (simple-string-p string)
	   (string-left-trim-simple-string-simple-string bag string)
	   (string-left-trim-simple-string-general-string bag string)))
      (string
       (if (simple-string-p string)
	   (string-left-trim-general-string-simple-string bag string)
	   (string-left-trim-general-string-general-string bag string)))
      (simple-vector
       (verify-vector-bag bag)
       (if (simple-string-p string)
	   (string-left-trim-simple-vector-simple-string bag string)
	   (string-left-trim-simple-vector-general-string bag string)))
      (vector
       (verify-vector-bag bag)
       (if (simple-string-p string)
	   (string-left-trim-general-vector-simple-string bag string)
	   (string-left-trim-general-vector-general-string bag string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-RIGHT-TRIM.

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; list, and a string represented as a simple string.
(defun string-right-trim-list-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-list-bag-p
		  (schar string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-list-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; list, and a string represented as a general string.
(defun string-right-trim-list-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-list-bag-p
		  (char string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-list-bag-p (char string i) character-bag)
		  return (extract-interval-general string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; simple-string, and a string represented as a simple string.
(defun string-right-trim-simple-string-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-string-bag-p
		  (schar string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-string-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; simple-string, and a string represented as a general string.
(defun string-right-trim-simple-string-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-string-bag-p
		  (char string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-string-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; general-string, and a string represented as a simple string.
(defun string-right-trim-general-string-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-string-bag-p
		  (schar string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-string-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; general-string, and a string represented as a general string.
(defun string-right-trim-general-string-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-string-bag-p
		  (char string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-string-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; simple vector, and a string represented as a simple string.
(defun string-right-trim-simple-vector-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-vector-bag-p
		  (schar string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-vector-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; simple vector, and a string represented as a general string.
(defun string-right-trim-simple-vector-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-simple-vector-bag-p
		  (char string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-vector-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; general vector, and a string represented as a simple string.
(defun string-right-trim-general-vector-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-vector-bag-p
		  (schar string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-vector-bag-p
			(schar string i)
			character-bag)
		  return (extract-interval-simple string 0 (1+ i))
		finally (return ""))))))

;;; A version of STRING-RIGHT-TRIM for a character bag represented as a
;;; general vector, and a string represented as a general string.
(defun string-right-trim-general-vector-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (not (character-in-general-vector-bag-p
		  (char string (1- length))
		  character-bag)))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-vector-bag-p
			(char string i)
			character-bag)
		  return (extract-interval-general string 0 (1+ i))
		finally (return ""))))))

(defun string-right-trim (character-bag string-designator)
  (let ((string (string string-designator))
	(bag character-bag))
    (etypecase bag
      (list
       (verify-list-bag bag)
       (if (simple-string-p string)
	   (string-right-trim-list-simple-string bag string)
	   (string-right-trim-list-general-string bag string)))
      (simple-string
       (if (simple-string-p string)
	   (string-right-trim-simple-string-simple-string bag string)
	   (string-right-trim-simple-string-general-string bag string)))
      (string
       (if (simple-string-p string)
	   (string-right-trim-general-string-simple-string bag string)
	   (string-right-trim-general-string-general-string bag string)))
      (simple-vector
       (verify-vector-bag bag)
       (if (simple-string-p string)
	   (string-right-trim-simple-vector-simple-string bag string)
	   (string-right-trim-simple-vector-general-string bag string)))
      (vector
       (verify-vector-bag bag)
       (if (simple-string-p string)
	   (string-right-trim-general-vector-simple-string bag string)
	   (string-right-trim-general-vector-general-string bag string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-TRIM.

;;; A version of STRING-TRIM for a character bag represented as a
;;; list, and a string represented as a simple string.
(defun string-trim-list-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-list-bag-p
		       (schar string 0)
		       character-bag))
		 (not (character-in-list-bag-p
		       (schar string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-list-bag-p
			(schar string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-list-bag-p
				       (schar string j)
				       character-bag)
				 return (extract-interval-simple string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; list, and a string represented as a general string.
(defun string-trim-list-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-list-bag-p
		       (char string 0)
		       character-bag))
		 (not (character-in-list-bag-p
		       (char string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-list-bag-p
			(char string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-list-bag-p
				       (char string j)
				       character-bag)
				 return (extract-interval-general string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; simple-string, and a string represented as a simple string.
(defun string-trim-simple-string-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-simple-string-bag-p
		       (schar string 0)
		       character-bag))
		 (not (character-in-simple-string-bag-p
		       (schar string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-string-bag-p
			(schar string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-simple-string-bag-p
				       (schar string j)
				       character-bag)
				 return (extract-interval-simple string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; simple-string, and a string represented as a general string.
(defun string-trim-simple-string-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-simple-string-bag-p
		       (char string 0)
		       character-bag))
		 (not (character-in-simple-string-bag-p
		       (char string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-string-bag-p
			(char string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-simple-string-bag-p
				       (char string j)
				       character-bag)
				 return (extract-interval-general string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; general-string, and a string represented as a simple string.
(defun string-trim-general-string-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-general-string-bag-p
		       (schar string 0)
		       character-bag))
		 (not (character-in-general-string-bag-p
		       (schar string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-string-bag-p
			(schar string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-general-string-bag-p
				       (schar string j)
				       character-bag)
				 return (extract-interval-simple string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; general-string, and a string represented as a general string.
(defun string-trim-general-string-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-general-string-bag-p
		       (char string 0)
		       character-bag))
		 (not (character-in-general-string-bag-p
		       (char string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-string-bag-p
			(char string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-general-string-bag-p
				       (char string j)
				       character-bag)
				 return (extract-interval-general string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; simple vector, and a string represented as a simple string.
(defun string-trim-simple-vector-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-simple-vector-bag-p
		       (schar string 0)
		       character-bag))
		 (not (character-in-simple-vector-bag-p
		       (schar string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-vector-bag-p
			(schar string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-simple-vector-bag-p
				       (schar string j)
				       character-bag)
				 return (extract-interval-simple string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; simple vector, and a string represented as a general string.
(defun string-trim-simple-vector-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-simple-vector-bag-p
		       (char string 0)
		       character-bag))
		 (not (character-in-simple-vector-bag-p
		       (char string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-simple-vector-bag-p
			(char string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-simple-vector-bag-p
				       (char string j)
				       character-bag)
				 return (extract-interval-general string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; general vector, and a string represented as a simple string.
(defun string-trim-general-vector-simple-string
    (character-bag string)
  (declare (type simple-string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-general-vector-bag-p
		       (schar string 0)
		       character-bag))
		 (not (character-in-general-vector-bag-p
		       (schar string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-vector-bag-p
			(schar string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-general-vector-bag-p
				       (schar string j)
				       character-bag)
				 return (extract-interval-simple string j (1+ i)))
		finally (return ""))))))

;;; A version of STRING-TRIM for a character bag represented as a
;;; general vector, and a string represented as a general string.
(defun string-trim-general-vector-general-string
    (character-bag string)
  (declare (type string string))
  (let ((length (length string)))
    (declare (type fixnum length))
    (if (or (zerop length)
	    (and (not (character-in-general-vector-bag-p
		       (char string 0)
		       character-bag))
		 (not (character-in-general-vector-bag-p
		       (char string (1- length))
		       character-bag))))
	string
	(locally
	    (declare (optimize (speed 3) (debug 0) (safety 0)))
	  (loop for i of-type fixnum downfrom (1- length) to 0
		unless (character-in-general-vector-bag-p
			(char string i)
			character-bag)
		  return (loop for j from 0 to i
			       unless (character-in-general-vector-bag-p
				       (char string j)
				       character-bag)
				 return (extract-interval-general string j (1+ i)))
		finally (return ""))))))

(defun string-trim (character-bag string-designator)
  (let ((string (string string-designator))
	(bag character-bag))
    (etypecase bag
      (list
       (verify-list-bag bag)
       (if (simple-string-p string)
	   (string-trim-list-simple-string bag string)
	   (string-trim-list-general-string bag string)))
      (simple-string
       (if (simple-string-p string)
	   (string-trim-simple-string-simple-string bag string)
	   (string-trim-simple-string-general-string bag string)))
      (string
       (if (simple-string-p string)
	   (string-trim-general-string-simple-string bag string)
	   (string-trim-general-string-general-string bag string)))
      (simple-vector
       (verify-vector-bag bag)
       (if (simple-string-p string)
	   (string-trim-simple-vector-simple-string bag string)
	   (string-trim-simple-vector-general-string bag string)))
      (vector
       (verify-vector-bag bag)
       (if (simple-string-p string)
	   (string-trim-general-vector-simple-string bag string)
	   (string-trim-general-vector-general-string bag string))))))
