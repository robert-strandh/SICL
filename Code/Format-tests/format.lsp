;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Feb 23 05:08:17 2004
;;;; Contains: Tests of FORMAT

(in-package :cl-test)


(defun def-format-test (name args result)
  `(deftest ,name
     (equalt
      (with-standard-io-syntax
       (with-output-to-string (s) (format s ,@args)))
      result)
     t))
