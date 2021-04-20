;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug  2 01:42:35 2004
;;;; Contains: Tests of printing using the ~A directive

(cl:in-package :cl-test)
(compile-and-load "printer-aux.lsp")

(def-format-test format.a.1
  "~a" (nil) "NIL")

(deftest format.a.2
  (with-standard-io-syntax
   (let ((*print-case* :downcase))
     (format nil "~A" nil)))
  "nil")

(deftest formatter.a.2
  (with-standard-io-syntax
   (let ((*print-case* :downcase))
     (formatter-call-to-string (formatter "~A") nil)))
  "nil")

(deftest format.a.3
  (with-standard-io-syntax
   (let ((*print-case* :capitalize))
     (format nil "~a" nil)))
  "Nil")

(deftest formatter.a.3
  (with-standard-io-syntax
   (let ((*print-case* :capitalize))
     (formatter-call-to-string (formatter "~a") nil)))
  "Nil")

(def-format-test format.a.4
  "~:a" (nil) "()")

(def-format-test format.a.5
  "~:A" ('(nil)) "(NIL)")

(def-format-test format.a.6
  "~:A" (#(nil)) "#(NIL)")

(deftest format.a.7
  (let ((fn (formatter "~a")))
    (loop for c across +standard-chars+
          for s1 = (string c)
          for s2 = (format nil "~a" s1)
          for s3 = (formatter-call-to-string fn s1)
          unless (and (string= s1 s2) (string= s2 s3))
          collect (list c s1 s2 s3)))
  nil)

(deftest format.a.8
  (let ((fn (formatter "~A")))
    (loop with count = 0
          for i from 0 below (min #x10000 char-code-limit)
          for c = (code-char i)
          for s1 = (and c (string c))
          for s2 = (and c (format nil "~A" s1))
          for s3 = (and c (formatter-call-to-string fn s1))
          unless (or (null c) (string= s1 s2) (string= s2 s3))
          do (incf count) and collect (list c s1 s2 s3)
          when (> count 100) collect "count limit exceeded" and do (loop-finish)))
  nil)

(deftest format.a.9
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~d@a" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "NIL"
  "NIL"
  "NIL"
  " NIL"
  "  NIL"
  "   NIL"
  "    NIL"
  "     NIL"
  "      NIL"
  "       NIL")

(deftest format.a.10
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~da" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "NIL"
  "NIL"
  "NIL"
  "NIL "
  "NIL  "
  "NIL   "
  "NIL    "
  "NIL     "
  "NIL      "
  "NIL       ")

(deftest format.a.11
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~d@:A" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "()"
  "()"
  " ()"
  "  ()"
  "   ()"
  "    ()"
  "     ()"
  "      ()"
  "       ()"
  "        ()")

(deftest format.a.12
  (with-standard-io-syntax
   (apply
    #'values
    (loop for i from 1 to 10
          for fmt = (format nil "~~~d:a" i)
          for s = (format nil fmt nil)
          for fn = (eval `(formatter ,fmt))
          for s2 = (formatter-call-to-string fn nil)
          do (assert (string= s s2))
          collect s)))
  "()"
  "()"
  "() "
  "()  "
  "()   "
  "()    "
  "()     "
  "()      "
  "()       "
  "()        ")

(deftest format.a.13
  (with-standard-io-syntax
   (apply
    #'values
    (let ((fn (formatter "~V:a")))
      (loop for i from 1 to 10
            for s = (format nil "~v:A" i nil)
            for s2 = (formatter-call-to-string fn i nil)
            do (assert (string= s s2))
            collect s))))
  "()"
  "()"
  "() "
  "()  "
  "()   "
  "()    "
  "()     "
  "()      "
  "()       "
  "()        ")

(deftest format.a.14
  (with-standard-io-syntax
   (apply
    #'values
    (let ((fn (formatter "~V@:A")))
      (loop for i from 1 to 10
            for s = (format nil "~v:@a" i nil)
            for s2 = (formatter-call-to-string fn i nil)
            do (assert (string= s s2))
            collect s))))
  "()"
  "()"
  " ()"
  "  ()"
  "   ()"
  "    ()"
  "     ()"
  "      ()"
  "       ()"
  "        ()")

(def-format-test format.a.15
  "~vA" (nil nil) "NIL")

(def-format-test format.a.16
  "~v:A" (nil nil) "()")

(def-format-test format.a.17
  "~@A" (nil) "NIL")

(def-format-test format.a.18
  "~v@A" (nil nil) "NIL")

(def-format-test format.a.19
  "~v:@a" (nil nil) "()")

(def-format-test format.a.20
  "~v@:a" (nil nil) "()")

;;; With colinc specified

(def-format-test format.a.21
  "~3,1a" (nil) "NIL")

(def-format-test format.a.22
  "~4,3a" (nil) "NIL   ")

(def-format-test format.a.23
  "~3,3@a" (nil) "NIL")

(def-format-test format.a.24
  "~4,4@a" (nil) "    NIL")

(def-format-test format.a.25
  "~5,3@a" (nil) "   NIL")

(def-format-test format.a.26
  "~5,3A" (nil) "NIL   ")

(def-format-test format.a.27
  "~7,3@a" (nil) "      NIL")

(def-format-test format.a.28
  "~7,3A" (nil) "NIL      ")

;;; With minpad

(deftest format.a.29
  (let ((fn (formatter "~v,,2A")))
    (loop for i from -4 to 10
          for s = (format nil "~v,,2A" i "ABC")
          for s2 = (formatter-call-to-string fn i "ABC")
          do (assert (string= s s2))
          collect s))
  ("ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "
   "ABC       "))

(def-format-test format.a.30
  "~3,,+2A" ("ABC") "ABC  ")

(def-format-test format.a.31
  "~3,,0A" ("ABC") "ABC")

(def-format-test format.a.32
  "~3,,-1A" ("ABC") "ABC")

(def-format-test format.a.33
  "~3,,0A" ("ABCD") "ABCD")

(def-format-test format.a.34
  "~3,,-1A" ("ABCD") "ABCD")

;;; With padchar

(def-format-test format.a.35
  "~4,,,'XA" ("AB") "ABXX")

(def-format-test format.a.36
  "~4,,,a" ("AB") "AB  ")

(def-format-test format.a.37
  "~4,,,'X@a" ("AB") "XXAB")

(def-format-test format.a.38
  "~4,,,@A" ("AB") "  AB")

(def-format-test format.a.39
  "~10,,,vA" (nil "abcde") "abcde     ")

(def-format-test format.a.40
  "~10,,,v@A" (nil "abcde") "     abcde")

(def-format-test format.a.41
  "~10,,,va" (#\* "abcde") "abcde*****")

(def-format-test format.a.42
  "~10,,,v@a" (#\* "abcde") "*****abcde")

;;; Other tests

(def-format-test format.a.43
  "~3,,vA" (nil "ABC") "ABC")

(deftest format.a.44
  (let ((fn (formatter "~3,,vA")))
    (loop for i from 0 to 6
          for s =(format nil "~3,,vA" i "ABC")
          for s2 = (formatter-call-to-string fn i "ABC")
          do (assert (string= s s2))
          collect s))
  ("ABC"
   "ABC "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "))

(deftest format.a.44a
  (let ((fn (formatter "~3,,v@A")))
    (loop for i from 0 to 6
          for s = (format nil "~3,,v@A" i "ABC")
          for s2 = (formatter-call-to-string fn i "ABC")
          do (assert (string= s s2))
          collect s))
  ("ABC"
   " ABC"
   "  ABC"
   "   ABC"
   "    ABC"
   "     ABC"
   "      ABC"))

(def-format-test format.a.45
  "~4,,va" (-1 "abcd") "abcd")

(def-format-test format.a.46
  "~5,vA" (nil "abc") "abc  ")

(def-format-test format.a.47
  "~5,vA" (3 "abc") "abc   ")

(def-format-test format.a.48
  "~5,v@A" (3 "abc") "   abc")

;;; # parameters

(def-format-test format.a.49
  "~#A" ("abc" nil nil nil) "abc " 3)

(def-format-test format.a.50
  "~#@a" ("abc" nil nil nil nil nil) "   abc" 5)

(def-format-test format.a.51
  "~5,#a" ("abc" nil nil nil) "abc    " 3)

(def-format-test format.a.52
  "~5,#@A" ("abc" nil nil nil) "    abc" 3)

(def-format-test format.a.53
  "~4,#A" ("abc" nil nil) "abc   " 2)

(def-format-test format.a.54
  "~4,#@A" ("abc" nil nil) "   abc" 2)

(def-format-test format.a.55
  "~#,#A" ("abc" nil nil nil) "abc    " 3)

(def-format-test format.a.56
  "~#,#@A" ("abc" nil nil nil) "    abc" 3)

(def-format-test format.a.57
  "~-100A" ("xyz") "xyz")

(def-format-test format.a.58
  "~-100000000000000000000a" ("xyz") "xyz")
