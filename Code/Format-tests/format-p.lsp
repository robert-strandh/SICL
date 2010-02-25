;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 17 21:32:45 2004
;;;; Contains: Tests of the ~P format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-format-test format.p.1
  "~p" (1) "")

(def-format-test format.p.2
  "~P" (2) "s")

(def-format-test format.p.3
  "~p" (0) "s")

(def-format-test format.p.4
  "~P" (1.0) "s")

(deftest format.p.5
  (loop for x in *universe*
	for s = (format nil "~p" x)
	unless (or (eql x 1) (string= s "s"))
	collect (list x s))
  nil)

(deftest formatter.p.5
  (let ((fn (formatter "~p")))
    (loop for x in *universe*
	  for s = (formatter-call-to-string fn x)
	  unless (or (eql x 1) (string= s "s"))
	  collect (list x s)))
  nil)

;;; :p

(def-format-test format.p.6
  "~D cat~:P" (1) "1 cat")

(def-format-test format.p.7
  "~D cat~:p" (2) "2 cats")

(def-format-test format.p.8
  "~D cat~:P" (0) "0 cats")

(def-format-test format.p.9
  "~D cat~:p" ("No") "No cats")

;;; :@p

(def-format-test format.p.10
  "~D penn~:@P" (1) "1 penny")

(def-format-test format.p.11
  "~D penn~:@p" (2) "2 pennies")

(def-format-test format.p.12
  "~D penn~@:P" (0) "0 pennies")

(def-format-test format.p.13
  "~D penn~@:p" ("No") "No pennies")

;;; @p

(def-format-test format.p.14
  "~@p" (1) "y")

(def-format-test format.p.15
  "~@P" (2) "ies")

(def-format-test format.p.16
  "~@p" (0) "ies")

(def-format-test format.p.17
  "~@P" (1.0) "ies")

(deftest format.p.18
  (loop for x in *universe*
	for s = (format nil "~@p" x)
	unless (or (eql x 1) (string= s "ies"))
	collect (list x s))
  nil)

(deftest formatter.p.18
  (let ((fn (formatter "~@P")))
    (loop for x in *universe*
	  for s = (formatter-call-to-string fn x)
	  unless (or (eql x 1) (string= s "ies"))
	  collect (list x s)))
  nil)
