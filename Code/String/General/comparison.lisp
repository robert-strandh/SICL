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

(defun first-mismatch-simple-simple-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-string string1)
		    (type simple-string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (schar string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-simple-general-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (stringp string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-string string1)
		    (type string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (schar string1 i) (char string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-simple-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (stringp string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type string string1)
		    (type simple-string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (char string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-general-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (stringp string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (stringp string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type string string1)
		    (type string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char= (char string1 i) (char string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-simple-simple-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-string string1)
		    (type simple-string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (schar string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-simple-general-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (stringp string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-string string1)
		    (type string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (schar string1 i) (char string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-simple-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (stringp string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type string string1)
		    (type simple-string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (char string1 i) (schar string2 j))
	    return i
	  finally (return i))))

(defun first-mismatch-general-general-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (stringp string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (stringp string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type string string1)
		    (type string string2)
		    (type fixnum start1 end1 start2 end2)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
	  for j of-type fixnum from start2 below end2
	  unless (char-equal (char string1 i) (char string2 j))
	    return i
	  finally (return i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING=.

(defun string=-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-simple-simple-char=
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string=-simple-general
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-simple-general-char=
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string=-general-simple
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-general-simple-char=
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string=-general-general
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-general-general-char=
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string=-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string=-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string=-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string=-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-EQUAL.

(defun string-equal-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-simple-simple-char-equal
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string-equal-simple-general
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-simple-general-char-equal
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string-equal-general-simple
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-general-simple-char-equal
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string-equal-general-general
    (string1 string2 start1 end1 start2 end2)
  (and (= (- end1 start1) (- end2 start2))
       (= (first-mismatch-general-general-char-equal
	   string1 string2 start1 end1 start2 end2)
	  end1)))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string-equal-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-equal-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string-equal-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-equal-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING<.

(defun string<-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (schar string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (schar string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (char string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (char string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string<-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string<-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string<-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string<-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-LESSP.

(defun string-lessp-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (schar string1 pos)
		       (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-lessp-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (schar string1 pos)
		       (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-lessp-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (char string1 pos)
		       (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-lessp-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is not less than
	       ;; STRING2.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (char string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string-lessp-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-lessp-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string-lessp-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-lessp-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING>.

(defun string>-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char> (schar string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string>-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char> (schar string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string>-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char> (char string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string>-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char> (char string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string>-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string>-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string>-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string>-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-GREATERP.

(defun string-greaterp-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-greaterp (schar string1 pos)
			  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-greaterp-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-greaterp (schar string1 pos)
			  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-greaterp-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-greaterp (char string1 pos)
			  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-greaterp-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is NOT greater than STRING2.
	   nil)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-greaterp (char string1 pos)
			  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is NOT greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string-greaterp-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-greaterp-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string-greaterp-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-greaterp-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING<=.

(defun string<=-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (schar string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<=-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (schar string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<=-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (char string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<=-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char< (char string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string<=-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string<=-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string<=-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string<=-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-NOT-GREATERP.

(defun string-not-greaterp-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (schar string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-not-greaterp-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (schar string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-not-greaterp-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (char string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-not-greaterp-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.  Then either we reached
	   ;; the end of STRING2 as well, in which case the two
	   ;; strings are equal, or we did NOT reach the end of
	   ;; STRING2, and STRING1 is a prefix of STRING2.  In both
	   ;; cases, STRING1 is less than or equal to STRING2.
	   pos)
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   nil)
	  ((char-lessp (char string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   pos)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   nil))))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string-not-greaterp-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-not-greaterp-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string-not-greaterp-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-not-greaterp-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING>=.

(defun string>=-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char< (schar string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string>=-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char< (schar string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string>=-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char< (char string1 pos)
		  (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string>=-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or equal to
	       ;; STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char< (char string1 pos)
		  (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string>=-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string>=-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string>=-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string>=-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-NOT-LESSP.

(defun string-not-lessp-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-lessp (schar string1 pos)
		       (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string-not-lessp-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-lessp (schar string1 pos)
		       (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string-not-lessp-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-lessp (char string1 pos)
		       (schar string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string-not-lessp-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.  Therefore STRING1 is greater than or
	       ;; equal to STRING2.
	       pos
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       nil))
	  ((= (- pos start1) (- end2 start2))
	   ;; We did not reach the end of STRING1, but we did reach
	   ;; the end of STRING2.  Then STRING1 is strictly greater
	   ;; than STRING2.
	   pos)
	  ((char-lessp (char string1 pos)
		       (char string2 (+ start2 (- pos start1))))
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is less than the character in
	   ;; STRING2.
	   nil)
	  (t
	   ;; We did not reach the end of either string, and the
	   ;; character in STRING1 is greater than the character in
	   ;; STRING2.
	   pos))))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string-not-lessp-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-not-lessp-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string-not-lessp-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-not-lessp-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING/=.

(defun string/=-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string/=-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string/=-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string/=-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char=
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string/=-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string/=-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string/=-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string/=-general-general
	     string1 string2 start1 end1 start2 end2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function STRING-NOT-EQUAL.

(defun string-not-equal-simple-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string-not-equal-simple-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-simple-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string-not-equal-general-simple
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-simple-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string-not-equal-general-general
    (string1 string2 start1 end1 start2 end2)
  (let ((pos (first-mismatch-general-general-char-equal
	      string1 string2 start1 end1 start2 end2)))
    (cond ((= pos end1)
	   ;; We reached the end of STRING1.
	   (if (= (- end1 start1) (- end2 start2))
	       ;; We also reached the end of STRING2, so the strings
	       ;; are equal.
	       nil
	       ;; There are more characters left in STRING2, so
	       ;; STRING1 is a prefix of STRING2.
	       pos))
	  (t
	   ;; We did not reach the end of STRING1, so the two strings
	   ;; must be different.
	   pos))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
	(string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (if (simple-string-p string1)
	(if (simple-string-p string2)
	    (string-not-equal-simple-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-not-equal-simple-general
	     string1 string2 start1 end1 start2 end2))
	(if (simple-string-p string2)
	    (string-not-equal-general-simple
	     string1 string2 start1 end1 start2 end2)
	    (string-not-equal-general-general
	     string1 string2 start1 end1 start2 end2)))))
