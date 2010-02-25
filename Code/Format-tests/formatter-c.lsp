;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Dec  5 14:32:46 2004
;;;; Contains: Tests of FORMATTER on the C directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest formatter.c.1
  (let ((fn (formatter "~C")))
    (loop
     for c across +standard-chars+
     when
     (let* (n
	    (ignored (loop for i below (random 5) collect i))
	    (s (with-output-to-string (stream)
				      (setq n (multiple-value-list
					       (apply fn stream c ignored))))))
       (unless (and (string= s (string c))
		    (equal n (list ignored)))
	 (list s ignored n)))
     collect it))
  nil)

(deftest formatter.c.1a
  (let ((fn (formatter "~c")))
    (loop
     with count = 0
     for i from 0 below (min #x10000 char-code-limit)
     for c = (code-char i)
     for ignored = (loop for j below (random 10) collect j)
     when
     (and c
	  (eql (char-code c) (char-int c))
	  (let* (n
		 (s (with-output-to-string
		      (stream)
		      (setq n (multiple-value-list
			       (apply fn stream c ignored))))))
	    (unless (and (string= s (string c))
			 (equal n (list ignored)))
	      (incf count)
	      (list i c s ignored n))))
     collect it
     when (> count 100) collect "count limit exceeded" and do (loop-finish)))
  nil)

(deftest formatter.c.2
  (let ((fn (formatter "~:C")))
    (loop
     for c across +standard-chars+
     when
     (and (graphic-char-p c)
	  (not (eql c #\Space))
	  (let* (n
		 (ignored (loop for i below (random 5) collect i))
		 (s (with-output-to-string (stream)
					   (setq n (multiple-value-list
						    (apply fn stream c ignored))))))
	    (unless (and (string= s (string c))
			 (equal n (list ignored)))
	      (list s ignored n))))
     collect it))
  nil)

(deftest formatter.c.2a
  (let ((fn (formatter "~:C")))
    (loop
     with count = 0
     for i from 0 below (min #x10000 char-code-limit)
     for c = (code-char i)
     for ignored = (loop for j below (random 10) collect j)
     when
     (and c
	  (eql (char-code c) (char-int c))
	  (graphic-char-p c)
	  (not (eql c #\Space))
	  (let* (n
		 (s (with-output-to-string
		      (stream)
		      (setq n (multiple-value-list
			       (apply fn stream c ignored))))))
	    (unless (and (string= s (string c))
			 (equal n (list ignored)))
	      (incf count)
	      (list i c s ignored n))))
     collect it
     when (> count 100) collect "count limit exceeded" and do (loop-finish)))
  nil)

(deftest formatter.c.4
  (let ((fn (formatter "~:C"))
	(n nil))
    (loop for c across +standard-chars+
	  for s = (with-output-to-string
		    (stream)
		    (setq n (multiple-value-list (funcall fn stream c))))
	  unless (or (graphic-char-p c)
		     (and (string= s (char-name c))
			  (equal n '(nil))))
	  collect (list c (char-name c) s)))
  nil)

(deftest formatter.c.4a
  (let ((fn (formatter "~:C"))
	(n nil))
    (loop for i from 0 below (min #x10000 char-code-limit)
	  for c = (code-char i)
	  for s = (and c
		       (with-output-to-string
			 (stream)
			 (setq n (multiple-value-list (funcall fn stream c 5)))))
	  unless (or (not c)
		     (graphic-char-p c)
		     (and (string= s (char-name c))
			  (equal n '((5)))))
	  collect (list c (char-name c) s)))
  nil)

(deftest formatter.c.5
  (let ((fn (formatter "~@C"))
	(n nil))
    (loop for c across +standard-chars+
	  for s = (with-output-to-string
		    (stream)
		    (setq n (multiple-value-list (funcall fn stream c 1 2 3))))
	  for c2 = (read-from-string s)
	  unless (and (eql c c2)
		      (equal n '((1 2 3))))
	  collect (list c s c2)))
  nil)

(deftest formatter.c.6
  (let ((n nil)
	(fn (formatter "~@:c")))
    (loop for c across +standard-chars+
	  for s1 = (with-output-to-string
		     (stream)
		     (setf n (multiple-value-list (funcall fn stream c 1 2))))
	  for s2 = (format nil "~:@C" c)
	  unless (and (eql (search s1 s2) 0) (equal n '((1 2))))
	  collect (list c s1 s2 n)))
  nil)
