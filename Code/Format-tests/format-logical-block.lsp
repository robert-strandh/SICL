;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug  8 12:17:31 2004
;;;; Contains: Tests of the ~< ~:> format directives

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; Error cases

;;; Prefix and suffix cannot contain format directives

(deftest format.logical-block.error.1
  (signals-error-always (format nil "~<foo~A~;~A~;bar~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.2
  (signals-error-always (format nil "~<foo~A~@;~A~;bar~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.3
  (signals-error-always (format nil "~<foo~;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.4
  (signals-error-always (format nil "~<foo~@;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.5
  (signals-error-always (format nil "~<foo~A~;~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.6
  (signals-error-always (format nil "~<foo~A~@;~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.7
  (signals-error-always (format nil "~<~;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.8
  (signals-error-always (format nil "~<~@;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.9
  (signals-error-always (format nil "~:<foo~A~;~A~;bar~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.10
  (signals-error-always (format nil "~:<foo~A~@;~A~;bar~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.11
  (signals-error-always (format nil "~:<foo~;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.12
  (signals-error-always (format nil "~:<foo~@;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.13
  (signals-error-always (format nil "~:<foo~A~;~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.14
  (signals-error-always (format nil "~:<foo~A~@;~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.15
  (signals-error-always (format nil "~:<~;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.16
  (signals-error-always (format nil "~:<~@;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.17
  (signals-error-always (format nil "~@<foo~A~;~A~;bar~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.18
  (signals-error-always (format nil "~@<foo~A~@;~A~;bar~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.19
  (signals-error-always (format nil "~@<foo~;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.20
  (signals-error-always (format nil "~@<foo~@;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.21
  (signals-error-always (format nil "~@<foo~A~;~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.22
  (signals-error-always (format nil "~@<foo~A~@;~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.23
  (signals-error-always (format nil "~@<~;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.24
  (signals-error-always (format nil "~@<~@;~A~;bar~A~:>" '(X) '(Y)) error)
  t t)

(deftest format.logical-block.error.25
  (signals-error-always (format nil "1~<X~<Y~:>Z~>2" nil nil nil) error)
  t t)

;;; "an error is also signaled if the ~<...~:;...~> form of ~<...~> is used
;;; in the same format string with ~W, ~_, ~<...~:>, ~I, or ~:T."

(deftest format.logical-block.error.26
  (signals-error-always (format nil "~<~:;~>~<~:>" nil nil nil) error)
  t t)

(deftest format.logical-block.error.27
  (signals-error-always (format nil "~<~:>~<~:;~>" nil nil nil) error)
  t t)

;;; Non-error tests

(def-pprint-test format.logical-block.1
  (format nil "~<~A~:>" '(nil))
  "NIL")

(def-pprint-test format.logical-block.2
  (format nil "~@<~A~:>" nil)
  "NIL")

(def-pprint-test format.logical-block.3
  (format nil "~:<~A~:>" '(nil))
  "(NIL)")

(def-pprint-test format.logical-block.4
  (format nil "~:@<~A~:>" nil)
  "(NIL)")

(def-pprint-test format.logical-block.5
  (format nil "~@:<~A~:>" nil)
  "(NIL)")

(def-pprint-test format.logical-block.6
  (format nil "~<~@{~A~^*~}~:>" '(1 2 3))
  "1*2*3")

(def-pprint-test format.logical-block.7
  (format nil "~:<~@{~A~^*~}~:>" '(1 2 3))
  "(1*2*3)")

(def-pprint-test format.logical-block.8
  (format nil "~:<~@{~A~^*~}~:>" 1)
  "1")

(def-pprint-test format.logical-block.9
  (format nil "~<~;~A~;~:>" '(1 2 3))
  "1")

(def-pprint-test format.logical-block.10
  (format nil "~<~;~A~:>" '(1 2 3))
  "1")

(def-pprint-test format.logical-block.11
  (format nil "~@<~;~A~;~:>" '(1 2 3))
  "(1 2 3)")

(def-pprint-test format.logical-block.12
  (format nil "~@<~;~A~:>" '(1 2 3))
  "(1 2 3)")

(def-pprint-test format.logical-block.13
  (format nil "~:<[~;~@{~A~^/~}~:>" '(1 2 3))
  "[1/2/3)")

(def-pprint-test format.logical-block.14
  (format nil "~:<~;~@{~A~^/~}~;]~:>" '(1 2 3))
  "1/2/3]")

(def-pprint-test format.logical-block.15
  (format nil "~:<[~;~@{~A~^/~}~;]~:>" '(1 2 3))
  "[1/2/3]")

(def-pprint-test format.logical-block.16
  (format nil "~@<~@{~A~^*~}~:>" 1 2 3)
  "1*2*3")

(def-pprint-test format.logical-block.17
  (format nil "~@<~@{~A~^ ~_~}~:>" 1 2 3)
  "1 2 3")

(def-pprint-test format.logical-block.18
  (format nil "~@<~@{~A~^ ~_~}~:>" 1 2 3)
  "1
2
3"
  :margin 2)

(def-pprint-test format.logical-block.19
  (format nil "~:@<~@{~A~^ ~_~}~:>" 1 2 3)
  "(1
 2
 3)"
  :margin 2)

(def-pprint-test format.logical-block.20
  (format nil "~@:<~@{~A~^ ~}~:>" 1 2 3)
  "(1 2 3)"
  :margin 2)

(def-pprint-test format.logical-block.21
  (format nil "~@:<~@{~A~^ ~:_~}~:>" 1 2 3)
  "(1
 2
 3)"
  :margin 2)

(def-pprint-test format.logical-block.22
  (format nil "~:@<~@{~A~^ ~}~:@>" 1 2 3)
  "(1
 2
 3)"
  :margin 2)

(def-pprint-test format.logical-block.23
  (format nil "~:@<~@{~A~^/~
                   ~}~:@>" 1 2 3)
  "(1/2/3)"
  :margin 2)

(def-pprint-test format.logical-block.24
  (format nil "~:@<~@{~A~^            ~:_~}~:>" 1 2 3)
  "(1
 2
 3)"
  :margin 2)

(def-pprint-test format.logical-block.25
  (format nil "~:@<~@{~A~^            ~}~:@>" 1 2 3)
  "(1
 2
 3)"
  :margin 2)

(def-pprint-test format.logical-block.26
  (format nil "~:@<~@{~A~^~}~:@>" "1 2 3")
  "(1 2 3)"
  :margin 2)

(def-pprint-test format.logical-block.27
  (format nil "~@<**~@;~@{~A~^       ~}~:@>" 1 2 3)
  "**1
**2
**3"
  :margin 3)

(def-pprint-test format.logical-block.28
  (format nil "~@<**~@;~@{~A~^       ~}~;XX~:@>" 1 2 3)
  "**1
**2
**3XX"
  :margin 3)

(def-pprint-test format.logical-block.29
  (format nil "~:@<**~@;~@{~A~^       ~}~:@>" 1 2 3)
  "**1
**2
**3)"
  :margin 3)


;;; Circularity detection

(def-pprint-test format.logical-block.circle.1
  (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0))) (list x x)))
  "(#1=(0) #1#)"
  :circle t)

(def-pprint-test format.logical-block.circle.2
  (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0))) (cons x x)))
  "(#1=(0) . #1#)"
  :circle t)

(def-pprint-test format.logical-block.circle.3
  (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0)))
				   (setf (cdr x) x)
				   x))
  "#1=(0 . #1#)"
  :circle t
  :len 500)

(def-pprint-test format.logical-block.circle.4
  (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0))) (list x x)))
  "((0) (0))")

(def-pprint-test format.logical-block.circle.5
  (format nil "~:<~@{~A~^ ~}~:>" (let ((x (list 0))) (cons x x)))
  "((0) 0)")

;;; ~^ terminates a logical block

(def-pprint-test format.logical-block.escape.1
  (format nil "~<~A~^xxxx~:>" '(1))
  "1")

(def-pprint-test format.logical-block.escape.2
  (format nil "~<~<~A~^xxx~:>yyy~:>" '((1)))
  "1yyy")
