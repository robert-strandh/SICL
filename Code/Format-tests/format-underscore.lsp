;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug  4 03:36:50 2004
;;;; Contains: Tests of the ~_ format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-ppblock-test format._.1
  (progn
    (dotimes (i 2) (write "A ") (pprint-newline :fill))
    ;; (write "B ") (pprint-newline :linear)
    (format t "B ~_")
    (dotimes (i 3) (write "A ") (pprint-newline :fill)))
  "A A B
A A A "
  :margin 10)

(def-ppblock-test format._.2
  (progn
   (dotimes (i 2) (write "A ") (pprint-newline :fill))
   ;; (write "B ") (pprint-newline :linear)
   (format t "B ~_")
   (dotimes (i 2) (write "C ") (pprint-newline :fill))
   (format t "D ~_")
   (dotimes (i 3) (write "A ") (pprint-newline :fill)))
  "A A B
C C D
A A A "
  :margin 10)

(def-ppblock-test format._.3
  (format t "A ~_A ~_A ~_A ~_")
  "A A A A "
  :margin 10)

(def-ppblock-test format._.4
  (format t "A ~_A ~_A ~_A ~_")
  "A A A A "
  :margin 10
  :miser 10)

(def-ppblock-test format._.5
  (format t "A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_A ~_")
  "A A A A A A A A A A "
  :margin 10
  :pretty nil)

(def-ppblock-test format._.6
  (dotimes (i 4) (format t "A             ~_"))
  "A
A
A
A
"
  :margin 10)

(def-ppblock-test format._.7
  (format t "A ~_A ~_A ~_A ~_~%A ~_A ~_A ~_A ~_")
  "A
A
A
A

A
A
A
A
"
  :margin 10)

(def-ppblock-test format._.8
  (progn
    (pprint-logical-block (*standard-output* nil)
			  (format t "A ~_A ~_A ~_A ~_"))
    (format t "~_")
    (pprint-logical-block (*standard-output* nil)
			  (format t "A ~_A ~_A ~_A ~_")))
  "A A A A
A A A A "
  :margin 10)

(deftest format._.9
  (with-output-to-string
    (s)
    (with-standard-io-syntax
     (let ((*print-readably* nil)
	   (*print-escape* nil)
	   (*print-pretty* t)
	   (*print-right-margin* 4)
	   (*print-miser-width* nil))
       (format s "A ~_A ~_A ~_A ~_A ~_"))))
  "A A A A A ")

(deftest formatter._.9
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t)
	 (*print-right-margin* 4)
	 (*print-miser-width* nil))
     (formatter-call-to-string
      (formatter "A ~_A ~_A ~_A ~_A ~_"))))
  "A A A A A ")

;;; miser

(def-ppblock-test format.@_.1
  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
  "A A A A A A A A A A "
  :margin 10)

(def-ppblock-test format.@_.2
  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
  "A A A A A A A A A A "
  :margin 10
  :miser 0)

(def-ppblock-test format.@_.3
  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
  "A A A A A A A A A A "
  :margin 10
  :miser 9)

(def-ppblock-test format.@_.4
  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
  "A
A
A
A
A
A
A
A
A
A
"
  :margin 10
  :miser 10)

(def-ppblock-test format.@_.5
  (format t "A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_A ~@_")
  "A A A A A A A A A A "
  :margin 10
  :miser 10
  :pretty nil)

(def-ppblock-test format.@_.6
  (format t "~%A~@_")
  "
A
"
  :margin 20
  :miser 20)

(def-ppblock-test format.@_.7
  (format t "~@_A~%")
  "
A
"
  :margin 20
  :miser 20)

(def-ppblock-test format.@_.8
  (progn
    (format t "AAAA ~_")
    (pprint-logical-block
     (*standard-output* nil)
     (format t "A ~@_A ~@_A ~@_A ~@_")))
  "AAAA
A A A A "
  :margin 10
  :miser 8)

(def-ppblock-test format.@_.9
  (progn
    (format t "AAAA ~:@_")
    (pprint-logical-block
     (*standard-output* nil)
     (format t "A ~@_A ~@_A ~@_A ~@_")))
  "AAAA
A A A A "
  :margin 10
  :miser 8)

(deftest format.@_.10
  (with-output-to-string
    (s)
    (with-standard-io-syntax
     (let ((*print-readably* nil)
	   (*print-escape* nil)
	   (*print-pretty* t)
	   (*print-right-margin* 4)
	   (*print-miser-width* 4))
       (format s "A ~@_A ~@_A ~@_A ~@_A ~@_"))))
  "A A A A A ")

(deftest formatter.@_.10
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t)
	 (*print-right-margin* 4)
	 (*print-miser-width* 4))
     (formatter-call-to-string
      (formatter "A ~@_A ~@_A ~@_A ~@_A ~@_"))))
  "A A A A A ")

;;; fill

(def-ppblock-test format.\:_.1
  (format t "A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_")
  "A A A A A
A A A A A "
  :margin 10)

(def-ppblock-test format.\:_.2
  (format t "A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_")
  "A A A
A A A
A A A
A "
  :margin 6)

(def-ppblock-test format.\:_.3
  (format t "A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_")
  "A A A
A A A
A A A
A "
  :margin 7)

(def-ppblock-test format.\:_.4
  (format t "A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_")
  "A A A A A
A A A A A "
  :margin 10
  :miser 9)

(def-ppblock-test format.\:_.5
  (format t "A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_A ~:_")
  "A
A
A
A
A
A
A
A
A
A
"
  :margin 10
  :miser 10)

(def-ppblock-test format.\:_.6
  (format t "~W~W~:_~W~W~:_~W~W~:_~W~W~:_~W~W~:_"
	  '(A B) #\Space
	  '(A B) #\Space
	  '(A B) #\Space
	  '(A B) #\Space
	  '(A B) #\Space)
  "(A B) (A B)
(A B) (A B)
(A B) "
  :margin 12)

(deftest format.\:_.7
  (with-output-to-string
    (s)
    (with-standard-io-syntax
     (let ((*print-readably* nil)
	   (*print-escape* nil)
	   (*print-right-margin* 4)
	   (*print-pretty* t)
	   (*print-miser-width* nil))
       (format s "A ~:_A ~:_A ~:_A ~:_A ~:_"))))
  "A A A A A ")

(deftest formatter.\:_.7
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-right-margin* 4)
	 (*print-pretty* t)
	 (*print-miser-width* nil))
     (formatter-call-to-string
      (formatter "A ~:_A ~:_A ~:_A ~:_A ~:_"))))
  "A A A A A ")

;;; mandatory

(def-ppblock-test format.\:@_.1
  (format t "A ~:@_A ~:@_A ~:@_A ~:@_")
  "A
A
A
A
")

(def-ppblock-test format.\:@_.2
  (format t "A ~@:_A ~@:_A ~@:_A ~@:_")
  "A
A
A
A
"
  :margin 10)

(def-ppblock-test format.\:@_.3
  (format t "A ~@:_A ")
  "A
A "
  :margin 1)

(def-ppblock-test format.\:@_.4
  (format t "A ~@:_A ~@:_A ~@:_A ~@:_")
  "A A A A "
  :pretty nil)

(deftest format.\:@_.5
  (with-output-to-string
    (s)
    (with-standard-io-syntax
     (let ((*print-readably* nil)
	   (*print-escape* nil)
	   (*print-pretty* t)
	   (*print-right-margin* 4)
	   (*print-miser-width* nil))
       (format s "A ~:@_A ~:@_A ~:@_A ~:@_A ~:@_"))))
  "A A A A A ")

(deftest formatter.\:@_.5
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil)
	 (*print-pretty* t)
	 (*print-right-margin* 4)
	 (*print-miser-width* nil))
     (formatter-call-to-string (formatter "A ~:@_A ~:@_A ~:@_A ~:@_A ~:@_"))))
  "A A A A A ")
