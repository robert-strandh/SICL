;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jul 28 00:27:00 2004
;;;; Contains: Tests of format directive ~~

(in-package :cl-test)
(compile-and-load "printer-aux.lsp")

(def-format-test format.~.1
  "~~" nil "~")

(deftest format.~.2
  (loop for i from 0 to 100
	for s = (make-string i :initial-element #\~)
	for format-string = (format nil "~~~D~~" i)
	for s2 = (format nil format-string)
	unless (string= s s2)
	collect (list i s s2))
  nil)

(deftest formatter.~.2
  (loop for i from 0 to 100
	for s = (make-string i :initial-element #\~)
	for format-string = (format nil "~~~D~~" i)
	for fn = (eval `(formatter ,format-string))
	for s2 = (formatter-call-to-string fn)
	unless (string= s s2)
	collect (list i s s2))
  nil)

(def-format-test format.~.3
  "~v~" (0) "")

(deftest format.~.4
  (loop for i from 0 to 100
	for s = (make-string i :initial-element #\~)
	for s2 = (format nil "~V~" i)
	unless (string= s s2)
	collect (list i s s2))
  nil)

(deftest formatter.~.4
  (let ((fn (formatter "~v~")))
    (loop for i from 0 to 100
	  for s = (make-string i :initial-element #\~)
	  for s2 = (formatter-call-to-string fn i)
	  unless (string= s s2)
	  collect (list i s s2)))
  nil)

(deftest format.~.5
  (loop for i from 0 to (min (- call-arguments-limit 3) 100)
	for s = (make-string i :initial-element #\~)
	for args = (make-list i)
	for s2 = (apply #'format nil "~#~" args)
	unless (string= s s2)
	collect (list i s s2))
  nil)

(deftest formatter.~.5
  (let ((fn (formatter "~#~")))
    (loop for i from 0 to (min (- call-arguments-limit 3) 100)
	  for s = (make-string i :initial-element #\~)
	  for args = (make-list i)
	  for s2 = (with-output-to-string
		     (stream)
		     (assert (equal (apply fn stream args) args)))
	  unless (string= s s2)
	  collect (list i s s2)))
  nil)
