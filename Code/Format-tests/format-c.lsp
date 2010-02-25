;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Jul 27 23:07:16 2004
;;;; Contains: Tests of formatted output, ~C directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; Test of the ~C directive

(deftest format.c.1
  (loop for c across +standard-chars+
	for s = (format nil "~C" c)
	unless (string= s (string c))
	collect (list c s))
  nil)

(deftest format.c.1a
  (loop with count = 0
	for i from 0 below (min #x10000 char-code-limit)
	for c = (code-char i)
	for s = (and c (format nil "~c" c))
	unless (or (not c)
		   (not (eql (char-code c) (char-int c)))
		   (string= s (string c)))
	do (incf count) and collect (list i c s)
	when (> count 100) collect "count limit exceeded" and do (loop-finish))
  nil)

(deftest format.c.2
  (loop for c across +standard-chars+
	for s = (format nil "~:c" c)
	unless (or (not (graphic-char-p c))
		   (eql c #\Space)
		   (string= s (string c)))
	collect (list c s))
  nil)

(deftest format.c.2a
  (loop with count = 0
	for i from 0 below (min #x10000 char-code-limit)
	for c = (code-char i)
	for s = (and c (format nil "~:C" c))
	unless (or (not c)
		   (not (eql (char-code c) (char-int c)))
		   (not (graphic-char-p c))
		   (eql c #\Space)
		   (string= s (string c)))
	do (incf count) and collect (list i c s)
	when (> count 100) collect "count limit exceeded" and do (loop-finish))
  nil)

(def-format-test format.c.3
  "~:C" (#\Space) #.(char-name #\Space))

(deftest format.c.4
  (loop for c across +standard-chars+
	for s = (format nil "~:C" c)
	unless (or (graphic-char-p c)
		   (string= s (char-name c)))
	collect (list c (char-name c) s))
  nil)

(deftest format.c.4a
  (loop with count = 0
	for i from 0 below (min #x10000 char-code-limit)
	for c = (code-char i)
	for s = (and c (format nil "~:c" c))
	unless (or (not c)
		   (not (eql (char-code c) (char-int c)))
		   (graphic-char-p c)
		   (string= s (char-name c)))
	do (incf count) and collect (print (list i c s))
	when (> count 100) collect "count limit exceeded" and do (loop-finish))
  nil)

(deftest format.c.5
  (loop for c across +standard-chars+
	for s = (format nil "~@c" c)
	for c2 = (read-from-string s)
	unless (eql c c2)
	collect (list c s c2))
  nil)

(deftest format.c.5a
  (loop with count = 0
	for i from 0 below (min #x10000 char-code-limit)
	for c = (code-char i)
	for s = (and c (format nil "~@C" c))
	for c2 = (and c (read-from-string s))
	unless (eql c c2)
	do (incf count) and collect (list c s c2)
	when (> count 100) collect "count limit exceeded" and do (loop-finish))
  nil)

(deftest format.c.6
  (loop for c across +standard-chars+
	for s1 = (format nil "~:C" c)
	for s2 = (format nil "~:@C" c)
	unless (eql (search s1 s2) 0)
	collect (list c s1 s2))
  nil)

(deftest format.c.6a
  (loop with count = 0
	for i from 0 below (min #x10000 char-code-limit)
	for c = (code-char i)
	for s1 = (and c (format nil "~:C" c))
	for s2 = (and c (format nil "~@:C" c))
	unless (or (not c) (eql (search s1 s2) 0))
	do (incf count) and collect (list c s1 s2)
	when (> count 100) collect "count limit exceeded" and do (loop-finish))
  nil)



