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
;;; Functions NSTRING-UPCASE and STRING-UPCASE.

(defun nstring-upcase-simple-base (string start end)
  (assert (typep string 'simple-base-string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (locally (declare (type simple-base-string string)
		    (type fixnum start end)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start below end
	  do (setf (schar string i) (char-upcase (schar string i)))))
  string)

(defun nstring-upcase-simple (string start end)
  (assert (simple-string-p string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (locally (declare (type simple-string string)
		    (type fixnum start end)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start below end
	  do (setf (schar string i) (char-upcase (schar string i)))))
  string)

(defun nstring-upcase (string &key (start 0) end)
  (declare (type string string))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (cond ((typep string 'simple-base-string)
	   (nstring-upcase-simple-base string start end))
	  ((simple-string-p string)
	   (nstring-upcase-simple string start end))))
  string)

(defun string-upcase (string-designator &key (start 0) end)
  (let* ((string (string string-designator))
	 (copy (copy-string string)))
    (nstring-upcase copy :start start :end end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions NSTRING-DOWNCASE and STRING-DOWNCASE.

(defun nstring-downcase-simple-base (string start end)
  (assert (typep string 'simple-base-string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (locally (declare (type simple-base-string string)
		    (type fixnum start end)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start below end
	  do (setf (schar string i) (char-downcase (schar string i)))))
  string)

(defun nstring-downcase-simple (string start end)
  (assert (simple-string-p string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (locally (declare (type simple-string string)
		    (type fixnum start end)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start below end
	  do (setf (schar string i) (char-downcase (schar string i)))))
  string)

(defun nstring-downcase (string &key (start 0) end)
  (declare (type string string))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (cond ((typep string 'simple-base-string)
	   (nstring-downcase-simple-base string start end))
	  ((simple-string-p string)
	   (nstring-downcase-simple string start end))))
  string)

(defun string-downcase (string-designator &key (start 0) end)
  (let* ((string (string string-designator))
	 (copy (copy-string string)))
    (nstring-downcase copy :start start :end end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions NSTRING-CAPITALIZE and STRING-CAPITALIZE.

(defun nstring-capitalize-simple-base (string start end)
  (assert (typep string 'simple-base-string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (locally (declare (type simple-base-string string)
		    (type fixnum start end)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop with state = nil
	  for i of-type fixnum from start below end
	  for char = (schar string i)
	  do (if state
		 (if (alphanumericp char)
		     (setf (schar string i) (char-downcase char))
		     (setf state nil))
		 (when (alphanumericp char)
		   (setf (schar string i) (char-upcase char))
		   (setf state t)))))
  string)

(defun nstring-capitalize-simple (string start end)
  (assert (simple-string-p string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (locally (declare (type simple-string string)
		    (type fixnum start end)
		    (optimize (speed 3) (safety 0) (debug 0)))
    (loop with state = nil
	  for i of-type fixnum from start below end
	  for char = (schar string i)
	  do (if state
		 (if (alphanumericp char)
		     (setf (schar string i) (char-downcase char))
		     (setf state nil))
		 (when (alphanumericp char)
		   (setf (schar string i) (char-upcase char))
		   (setf state t)))))
  string)

(defun nstring-capitalize (string &key (start 0) end)
  (declare (type string string))
  (let ((length (length string)))
    (when (null end) (setf end length))
    (check-bounding-indices string start end)
    (cond ((typep string 'simple-base-string)
	   (nstring-capitalize-simple-base string start end))
	  ((simple-string-p string)
	   (nstring-capitalize-simple string start end))))
  string)

(defun string-capitalize (string-designator &key (start 0) end)
  (let* ((string (string string-designator))
	 (copy (copy-string string)))
    (nstring-capitalize copy :start start :end end)))
