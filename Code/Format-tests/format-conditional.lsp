;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 25 19:27:25 2004
;;;; Contains: Tests of the ~[ ~] forms

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-format-test format.cond.1
  "~[~]" (0) "")

(def-format-test format.cond.2
  "~[a~]" (0) "a")

(def-format-test format.cond.3
  "~[a~]" (-1) "")

(def-format-test format.cond.4
  "~[a~]" ((1- most-negative-fixnum)) "")

(def-format-test format.cond.5
  "~[a~]" (1) "")

(def-format-test format.cond.6
  "~[a~]" ((1+ most-positive-fixnum)) "")

(deftest format.cond.7
  (loop for i from -1 to 10
	collect (format nil "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest formatter.cond.7
  (let ((fn (formatter "~[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
    (loop for i from -1 to 10
	  collect (formatter-call-to-string fn i)))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(def-format-test format.cond.8
  "~0[a~;b~;c~;d~]" (3) "a" 1)

(def-format-test format.cond.9
  "~-1[a~;b~;c~;d~]" (3) "" 1)

(def-format-test format.cond.10
  "~1[a~;b~;c~;d~]" (3) "b" 1)

(def-format-test format.cond.11
  "~4[a~;b~;c~;d~]" (3) "" 1)

(def-format-test format.cond.12
  "~100000000000000000000000000000000[a~;b~;c~;d~]" (3) "" 1)

(deftest format.cond.13
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" i nil))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest formatter.cond.13
  (let ((fn (formatter "~V[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
    (loop for i from -1 to 10
	  collect (formatter-call-to-string fn i)))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest format.cond.14
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]" nil i))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(deftest formatter.cond.14
  (let ((fn (formatter "~v[a~;b~;c~;d~;e~;f~;g~;h~;i~]")))
    (loop for i from -1 to 10
	  collect (formatter-call-to-string fn nil i)))
  ("" "a" "b" "c" "d" "e" "f" "g" "h" "i" "" ""))

(def-format-test format.cond.15
  "~#[A~;B~]" nil "A")

(def-format-test format.cond.16
  "~#[A~;B~]" (nil) "B" 1)

;;; ~[ .~:;  ~]

(deftest format.cond\:.1
  (loop for i from -100 to 100
	for s = (format nil "~[~:;a~]" i)
	unless (or (zerop i) (string= s "a"))
	collect (list i s))
  nil)

(deftest formatter.cond\:.1
  (let ((fn (formatter "~[~:;a~]")))
    (loop for i from -100 to 100
	  for s = (formatter-call-to-string fn i)
	  unless (or (zerop i) (string= s "a"))
	  collect (list i s)))
  nil)

(def-format-test format.cond\:.2
  "~[a~:;b~]" (0) "a")

(def-format-test format.cond\:.3
  "~[a~:;b~]" ((1- most-negative-fixnum)) "b")

(def-format-test format.cond\:.4
  "~[a~:;b~]" ((1+ most-positive-fixnum)) "b")

(deftest format.cond\:.5
  (loop for i from -1 to 10
	collect (format nil "~[a~;b~;c~;d~:;e~]" i))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest formatter.cond\:.5
  (let ((fn (formatter "~[a~;b~;c~;d~:;e~]")))
    (loop for i from -1 to 10
	  collect (formatter-call-to-string fn i)))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest format.cond\:.6
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~:;e~]" i nil))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest formatter.cond\:.6
  (let ((fn (formatter "~v[a~;b~;c~;d~:;e~]")))
    (loop for i from -1 to 10
	  collect (formatter-call-to-string fn i)))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest format.cond\:.7
  (loop for i from -1 to 10
	collect (format nil "~v[a~;b~;c~;d~:;e~]" nil i))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(deftest formatter.cond\:.7
  (let ((fn (formatter "~v[a~;b~;c~;d~:;e~]")))
    (loop for i from -1 to 10
	  collect (formatter-call-to-string fn nil i)))
  ("e" "a" "b" "c" "d" "e" "e" "e" "e" "e" "e" "e"))

(def-format-test format.cond\:.8
  "~#[A~:;B~]" nil "A")

(def-format-test format.cond\:.9
  "~#[A~:;B~]" (nil nil) "B" 2)

;;; ~:[...~]

(def-format-test format.\:cond.1
  "~:[a~;b~]" (nil) "a")

(deftest format.\:cond.2
  (loop for x in *mini-universe*
	for s = (format nil "~:[a~;b~]" x)
	when (and x (not (string= s "b")))
	collect (list x s))
  nil)

(deftest formatter.\:cond.2
  (let ((fn (formatter "~:[a~;b~]")))
    (loop for x in *mini-universe*
	  for s = (formatter-call-to-string fn x)
	  when (and x (not (string= s "b")))
	  collect (list x s)))
  nil)

;;; ~@[ ... ~]

(def-format-test format.@cond.1
  "~@[X~]Y~A" (1) "XY1")

(def-format-test format.@cond.2
  "~@[X~]Y~A" (nil 2) "Y2")
