;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug  3 11:55:07 2004
;;;; Contains: Test of the ~S format directive

(in-package :cl-test)
(compile-and-load "printer-aux.lsp")

(deftest format.s.1
  (let ((*print-readably* nil)
	(*print-case* :upcase))
    (format nil "~s" nil))
  "NIL")

(deftest formatter.s.1
  (let ((*print-readably* nil)
	(*print-case* :upcase))
    (formatter-call-to-string (formatter "~s") nil))
  "NIL")

(def-format-test format.s.2
  "~:s" (nil) "()")

(deftest format.s.3
  (let ((*print-readably* nil)
	(*print-case* :upcase))
    (format nil "~:s" '(nil)))
  "(NIL)")

(deftest formatter.s.3
  (let ((*print-readably* nil)
	(*print-case* :upcase))
    (formatter-call-to-string (formatter "~:s") '(nil)))
  "(NIL)")

(deftest format.s.4
  (let ((*print-readably* nil)
	(*print-case* :downcase))
    (format nil "~s" 'nil))
  "nil")

(deftest formatter.s.4
  (let ((*print-readably* nil)
	(*print-case* :downcase))
    (formatter-call-to-string (formatter "~s") 'nil))
  "nil")

(deftest format.s.5
  (let ((*print-readably* nil)
	(*print-case* :capitalize))
    (format nil "~s" 'nil))
  "Nil")

(deftest formatter.s.5
  (let ((*print-readably* nil)
	(*print-case* :capitalize))
    (formatter-call-to-string (formatter "~s") 'nil))
  "Nil")

(def-format-test format.s.6
  "~:s" (#(nil)) "#(NIL)")

(deftest format.s.7
  (let ((fn (formatter "~S")))
    (with-standard-io-syntax
     (let ((*print-readably* nil))
       (loop for c across +standard-chars+
	     for s = (format nil "~S" c)
	     for s2 = (formatter-call-to-string fn c)
	     for c2 = (read-from-string s)
	     unless (and (eql c c2) (string= s s2))
	     collect (list c s c2 s2)))))
  nil)

(deftest format.s.8
  (let ((fn (formatter "~s")))
    (with-standard-io-syntax
     (let ((*print-readably* nil))
       (loop with count = 0
	     for i from 0 below (min #x10000 char-code-limit)
	     for c = (code-char i)
	     for s1 = (and c (format nil "#\\~:c" c))
	     for s2 = (and c (format nil "~S" c))
	     for s3 = (formatter-call-to-string fn c)
	     unless (or (null c)
			(graphic-char-p c)
			(and (string= s1 s2) (string= s2 s3)))
	      do (incf count) and collect (list c s1 s2)
	     when (> count 100)
	      collect "count limit exceeded"
	      and do (loop-finish)))))
  nil)

(deftest format.s.9
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for fmt = (format nil "~~~d@s" i)
	    for s = (format nil fmt nil)
	    for fn = (eval `(formatter ,fmt))
	    for s2 = (formatter-call-to-string fn nil)
	    do (assert (string= s s2))
	    collect s))))
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

(deftest format.s.10
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for fmt = (format nil "~~~dS" i)
	    for s = (format nil fmt nil)
	    for fn = (eval `(formatter ,fmt))
	    for s2 = (formatter-call-to-string fn nil)
	    do (assert (string= s s2))
	    collect s))))
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

(deftest format.s.11
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for fmt = (format nil "~~~d@:S" i)
	    for s = (format nil fmt nil)
	    for fn = (eval `(formatter ,fmt))
	    for s2 = (formatter-call-to-string fn nil)
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

(deftest format.s.12
  (with-standard-io-syntax
   (let ((*print-readably* nil))
     (apply
      #'values
      (loop for i from 1 to 10
	    for fmt = (format nil "~~~d:s" i)
	    for s = (format nil fmt nil)
	    for fn = (eval `(formatter ,fmt))
	    for s2 = (formatter-call-to-string fn nil)
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

(deftest format.s.13
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (fn (formatter "~V:s")))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s = (format nil "~v:S" i nil)
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

(deftest format.s.14
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (fn (formatter "~V@:s")))
     (apply
      #'values
      (loop for i from 1 to 10
	    for s = (format nil "~v:@s" i nil)
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

(def-format-test format.s.15
  "~vS" (nil nil) "NIL")

(def-format-test format.s.16
  "~v:S" (nil nil) "()")

(def-format-test format.s.17
  "~@S" (nil) "NIL")

(def-format-test format.s.18
  "~v@S" (nil nil) "NIL")

(def-format-test format.s.19
  "~v:@s" (nil nil) "()")

(def-format-test format.s.20
  "~v@:s" (nil nil) "()")

;;; With colinc specified

(def-format-test format.s.21
  "~3,1s" (nil) "NIL")

(def-format-test format.s.22
  "~4,3s" (nil) "NIL   ")

(def-format-test format.s.23
  "~3,3@s" (nil) "NIL")

(def-format-test format.s.24
  "~4,4@s" (nil) "    NIL")

(def-format-test format.s.25
  "~5,3@s" (nil) "   NIL")

(def-format-test format.s.26
  "~5,3S" (nil) "NIL   ")

(def-format-test format.s.27
  "~7,3@s" (nil) "      NIL")

(def-format-test format.s.28
  "~7,3S" (nil) "NIL      ")

;;; With minpad

(deftest format.s.29
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (fn (formatter "~V,,2s")))
     (loop for i from -4 to 10
	   for s = (format nil "~v,,2S" i 'ABC)
	   for s2 = (formatter-call-to-string fn i 'ABC)
	   do (assert (string= s s2))
	   collect s)))
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

(def-format-test format.s.30
  "~3,,+2S" ('ABC) "ABC  ")

(def-format-test format.s.31
  "~3,,0S" ('ABC) "ABC")

(def-format-test format.s.32
  "~3,,-1S" ('ABC) "ABC")

(def-format-test format.s.33
  "~3,,0S" ('ABCD) "ABCD")

(def-format-test format.s.34
  "~3,,-1S" ('ABCD) "ABCD")

;;; With padchar

(def-format-test format.s.35
  "~4,,,'XS" ('AB) "ABXX")

(def-format-test format.s.36
  "~4,,,s" ('AB) "AB  ")

(def-format-test format.s.37
  "~4,,,'X@s" ('AB) "XXAB")

(def-format-test format.s.38
  "~4,,,@S" ('AB) "  AB")

(def-format-test format.s.39
  "~10,,,vS" (nil 'ABCDE) "ABCDE     ")

(def-format-test format.s.40
  "~10,,,v@S" (nil 'ABCDE) "     ABCDE")

(def-format-test format.s.41
  "~10,,,vs" (#\* 'ABCDE) "ABCDE*****")

(def-format-test format.s.42
  "~10,,,v@s" (#\* 'ABCDE) "*****ABCDE")

;;; Other tests

(def-format-test format.s.43
  "~3,,vS" (nil 246) "246")

(deftest format.s.44
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (fn (formatter "~3,,vs")))
     (loop for i from 0 to 6
	   for s = (format nil "~3,,vS" i 'ABC)
	   for s2 = (formatter-call-to-string fn i 'ABC)
	   do (assert (string= s s2))
	   collect s)))
  ("ABC"
   "ABC "
   "ABC  "
   "ABC   "
   "ABC    "
   "ABC     "
   "ABC      "))

(deftest format.s.44a
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*package* (find-package :cl-test))
	 (fn (formatter "~3,,V@S")))
     (loop for i from 0 to 6
	   for s = (format nil "~3,,v@S" i 'ABC)
	   for s2 = (formatter-call-to-string fn i 'ABC)
	   do (assert (string= s s2))
	   collect s)))
  ("ABC"
   " ABC"
   "  ABC"
   "   ABC"
   "    ABC"
   "     ABC"
   "      ABC"))

(def-format-test format.s.45
  "~4,,vs" (-1 1234) "1234")

(def-format-test format.s.46
  "~5,vS" (nil 123) "123  ")

(def-format-test format.s.47
  "~5,vS" (3 456) "456   ")

(def-format-test format.s.48
  "~5,v@S" (3 789) "   789")
