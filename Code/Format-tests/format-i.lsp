;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 21 07:01:36 2004
;;;; Contains: Tests for the ~I format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

;;; pprint-indent.9
(def-pprint-test format.i.1
  (format nil "~<M~3:i~:@_M~:>" '(M M))
  "M
    M")

;;; See pprint-indent.10
(def-pprint-test format.i.2
  (format nil "~:<M~1:I~@:_M~:>" '(M M))
  "(M
   M)")

;;; See pprint-indent.11
(def-pprint-test format.i.3
  (format nil "~<(~;M~-1:i~:@_M~;)~:>" '(M M))
  "(M
 M)")

(def-pprint-test format.i.4
  (format nil "~:<M~-1:i~:@_M~:>" '(M M))
  "(M
 M)")

(def-pprint-test format.i.5
  (format nil "~<(~;M~:I~:@_M~;)~:>" '(M M))
  "(M
  M)")

(def-pprint-test format.i.6
  (format nil "~<(~;M~v:i~:@_M~;)~:>" '(nil))
  "(M
  M)")

(def-pprint-test format.i.7
  (format nil "~:<M~-2:i~:@_M~:>" '(M M))
  "(M
M)")

(def-pprint-test format.i.8
  (format nil "~<M~:i~:@_M~:>" '(M M))
  "M
 M")

;;; See pprint-indent.13
(def-pprint-test format.i.9
  (format nil "~<MMM~I~:@_MMMMM~:>" '(M M))
  "MMM
MMMMM")

(def-pprint-test format.i.10
  (format nil "~:<MMM~I~:@_MMMMM~:>" '(M M))
  "(MMM
 MMMMM)")

(def-pprint-test format.i.11
  (format nil "~<MMM~1I~:@_MMMMM~:>" '(M M))
  "MMM
 MMMMM")

(def-pprint-test format.i.12
  (format nil "XXX~<MMM~1I~:@_MMMMM~:>" '(M M))
  "XXXMMM
    MMMMM")

(def-pprint-test format.i.13
  (format nil "XXX~<MMM~I~:@_MMMMM~:>" '(M M))
  "XXXMMM
   MMMMM")

(def-pprint-test format.i.14
  (format nil "XXX~<MMM~-1I~:@_MMMMM~:>" '(M M))
  "XXXMMM
  MMMMM")

(def-pprint-test format.i.15
  (format nil "XXX~<MMM~vI~:@_MMMMM~:>" '(nil))
  "XXXMMM
   MMMMM")

(def-pprint-test format.i.16
  (format nil "XXX~<MMM~vI~:@_MMMMM~:>" '(2))
  "XXXMMM
     MMMMM")
