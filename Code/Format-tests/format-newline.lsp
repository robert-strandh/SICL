;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 27 08:07:16 2004
;;;; Contains: Tests of ~<newline>

(in-package :cl-test)
(compile-and-load "printer-aux.lsp")

(def-format-test format.newline.1
  (concatenate 'string "~" (string #\Newline) "   X")
  nil "X")

(def-format-test format.newline.2
  (concatenate 'string "A~:" (string #\Newline) " X")
  nil "A X")

(def-format-test format.newline.3
  (concatenate 'string "A~@" (string #\Newline) " X")
  nil #.(concatenate 'string "A" (string #\Newline) "X"))

