;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 20:28:24 2004
;;;; Contains: Tests of the ~( format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-format-test format.paren.1
  "~(XXyy~AuuVV~)" ("ABc dEF ghI") "xxyyabc def ghiuuvv")

;;; Conversion of simple characters to downcase
(deftest format.paren.2
  (loop for i from 0 below (min char-code-limit (ash 1 16))
	for c = (code-char i)
	when (and c
		  (eql (char-code c) (char-int c))
		  (upper-case-p c)
		  (let ((s1 (format nil "~(~c~)" c))
			(s2 (string (char-downcase c))))
		    (if
			(or (not (eql (length s1) 1))
			    (not (eql (length s2) 1))
			    (not (eql (elt s1 0)
				      (elt s2 0))))
			(list i c s1 s2)
		      nil)))
	collect it)
  nil)

(deftest formatter.paren.2
  (let ((fn (formatter "~(~c~)")))
    (loop for i from 0 below (min char-code-limit (ash 1 16))
	  for c = (code-char i)
	  when (and c
		    (eql (char-code c) (char-int c))
		    (upper-case-p c)
		    (let ((s1 (formatter-call-to-string fn c))
			  (s2 (string (char-downcase c))))
		      (if
			  (or (not (eql (length s1) 1))
			      (not (eql (length s2) 1))
			      (not (eql (elt s1 0)
					(elt s2 0))))
			  (list i c s1 s2)
			nil)))
	  collect it))
  nil)


(def-format-test format.paren.3
  "~@(this is a TEST.~)" nil "This is a test.")

(def-format-test format.paren.4
  "~@(!@#$%^&*this is a TEST.~)" nil "!@#$%^&*This is a test.")

(def-format-test format.paren.5
  "~:(this is a TEST.~)" nil "This Is A Test.")
			
(def-format-test format.paren.6
  "~:(this is7a TEST.~)" nil "This Is7a Test.")

(def-format-test format.paren.7
  "~:@(this is AlSo A teSt~)" nil "THIS IS ALSO A TEST")

(deftest format.paren.8
  (loop for i from 0 below (min char-code-limit (ash 1 16))
	for c = (code-char i)
	when (and c
		  (eql (char-code c) (char-int c))
		  (lower-case-p c)
		  (let ((s1 (format nil "~@:(~c~)" c))
			(s2 (string (char-upcase c))))
		    (if
			(or (not (eql (length s1) 1))
			    (not (eql (length s2) 1))
			    (not (eql (elt s1 0)
				      (elt s2 0))))
			(list i c s1 s2)
		      nil)))
	collect it)
  nil)

(deftest formatter.paren.8
  (let ((fn (formatter "~@:(~c~)")))
    (loop for i from 0 below (min char-code-limit (ash 1 16))
	  for c = (code-char i)
	  when (and c
		    (eql (char-code c) (char-int c))
		    (lower-case-p c)
		    (let ((s1 (formatter-call-to-string fn c))
			  (s2 (string (char-upcase c))))
		      (if
			  (or (not (eql (length s1) 1))
			      (not (eql (length s2) 1))
			      (not (eql (elt s1 0)
					(elt s2 0))))
			  (list i c s1 s2)
			nil)))
	  collect it))
  nil)

;;; Nested conversion

(def-format-test format.paren.9
  "~(aBc ~:(def~) GHi~)" nil "abc def ghi")

(def-format-test format.paren.10
  "~(aBc ~(def~) GHi~)" nil "abc def ghi")

(def-format-test format.paren.11
  "~@(aBc ~:(def~) GHi~)" nil "Abc def ghi")

(def-format-test format.paren.12
  "~(aBc ~@(def~) GHi~)" nil "abc def ghi")

(def-format-test format.paren.13
  "~(aBc ~:(def~) GHi~)" nil "abc def ghi")

(def-format-test format.paren.14
  "~:(aBc ~(def~) GHi~)" nil "Abc Def Ghi")

(def-format-test format.paren.15
  "~:(aBc ~:(def~) GHi~)" nil "Abc Def Ghi")

(def-format-test format.paren.16
  "~:(aBc ~@(def~) GHi~)" nil "Abc Def Ghi")

(def-format-test format.paren.17
  "~:(aBc ~@:(def~) GHi~)" nil "Abc Def Ghi")

(def-format-test format.paren.18
  "~@(aBc ~(def~) GHi~)" nil "Abc def ghi")

(def-format-test format.paren.19
  "~@(aBc ~:(def~) GHi~)" nil "Abc def ghi")

(def-format-test format.paren.20
  "~@(aBc ~@(def~) GHi~)" nil "Abc def ghi")

(def-format-test format.paren.21
  "~@(aBc ~@:(def~) GHi~)" nil "Abc def ghi")

(def-format-test format.paren.22
  "~:@(aBc ~(def~) GHi~)" nil "ABC DEF GHI")

(def-format-test format.paren.23
  "~@:(aBc ~:(def~) GHi~)" nil "ABC DEF GHI")

(def-format-test format.paren.24
  "~:@(aBc ~@(def~) GHi~)" nil "ABC DEF GHI")

(def-format-test format.paren.25
  "~@:(aBc ~@:(def~) GHi~)" nil "ABC DEF GHI")
