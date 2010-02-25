;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 21 12:45:22 2004
;;;; Contains: Tests of the ~T format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(def-pprint-test format.t.1
  (format nil "~0,0T")
  "")

(def-pprint-test format.t.2
  (format nil "~1,0T")
  " ")

(def-pprint-test format.t.3
  (format nil "~0,1T")
  " ")

(def-pprint-test format.t.4
  (loop for i from 0 to 20
	for s = (format nil "~0,vT" i)
	unless (string= s (make-string i :initial-element #\Space))
	collect (list i s))
  nil)

(def-pprint-test format.t.5
  (loop for i from 0 to 20
	for s = (format nil "~v,0T" i)
	unless (string= s (make-string i :initial-element #\Space))
	collect (list i s))
  nil)

(def-pprint-test format.t.6
  (loop for n1 = (random 30)
	for s1 = (make-string n1 :initial-element #\X)
	for n2 = (random 30)
	for inc = (random 20)
	for s2 = (cond
		  ((< n1 n2)
		   (concatenate 'string s1 (make-string (- n2 n1)
							:initial-element #\Space)))
		  ((= inc 0) s1)
		  (t (loop do (incf n2 inc) while (<= n2 n1))
		     (concatenate 'string s1 (make-string (- n2 n1)
							  :initial-element #\Space))))
	for pretty = (coin)
	for result = (let ((*print-pretty* pretty))
		       (format nil (format nil "~A~~~D,~DT" s1 n2 inc)))
	repeat 100
	unless (string= s2 result)
	collect (list n1 n2 inc pretty s2 result))
  nil)

(def-pprint-test format.t.7
  (loop for n1 = (random 30)
	for s1 = (make-string n1 :initial-element #\X)
	for n2 = (random 30)
	for inc = (random 20)
	for s2 = (cond
		  ((< n1 n2)
		   (concatenate 'string s1 (make-string (- n2 n1)
							:initial-element #\Space)))
		  ((= inc 0) s1)
		  (t (loop do (incf n2 inc) while (<= n2 n1))
		     (concatenate 'string s1 (make-string (- n2 n1)
							  :initial-element #\Space))))
	for pretty = (coin)
	for result = (let ((*print-pretty* pretty))
		       (format nil "~A~v,vt" s1 n2 inc))
	repeat 100
	unless (string= s2 result)
	collect (list n1 n2 inc pretty s2 result))
  nil)

(def-pprint-test format.t.8
  (loop for i from 1 to 20
	for s = (format nil " ~v,vT" nil i)
	unless (string= s (make-string (1+ i) :initial-element #\Space))
	collect (list i s))
  nil)

(def-pprint-test format.t.9
  (loop for i from 1 to 20
	for s = (format nil "~v,vT" i nil)
	unless (string= s (make-string i :initial-element #\Space))
	collect (list i s))
  nil)

(def-pprint-test format.t.10
  (format nil "XXXXX~2,0T")
  "XXXXX")

;;; @t

(def-pprint-test format.@t.1
  (format nil "~1,1@t")
  " ")

(def-pprint-test format.@t.2
  (loop for colnum from 0 to 20
	for s1 = (format nil "~v,1@t" colnum)
	for s2 = (make-string colnum :initial-element #\Space)
	unless (string= s1 s2)
	collect (list colnum s1 s2))
  nil)

(def-pprint-test format.@t.3
  (loop for colnum = (random 50)
	for colinc = (1+ (random 20))
	for s1 = (format nil "~v,v@t" colnum colinc)
	for s2 = (make-string (* colinc (ceiling colnum colinc))
			      :initial-element #\Space)
	repeat 100
	unless (string= s1 s2)
	collect (list colnum colinc s1 s2))
  nil)

(def-pprint-test format.@t.4
  (loop for colnum = (random 50)
	for colinc = (1+ (random 20))
	for s1 = (format nil "~v,1@T~0,v@t" colnum colinc)
	for s2 = (make-string (* colinc (ceiling colnum colinc))
			      :initial-element #\Space)
	repeat 100
	unless (string= s1 s2)
	collect (list colnum colinc s1 s2))
  nil)

(def-pprint-test format.@t.5
  (loop for colnum = (random 50)
	for colinc = (1+ (random 20))
	for pretty = (coin)
	for s1 = (let ((*pretty* pretty))
		   (format nil (format nil "~~~d,~d@t" colnum colinc)))
	for s2 = (make-string (* colinc (ceiling colnum colinc))
			      :initial-element #\Space)
	repeat 100
	unless (string= s1 s2)
	collect (list colnum colinc pretty s1 s2))
  nil)

;;; Pretty printing (colon modifier)

;;; Not a pretty printing stream

(def-pprint-test format.\:t.1
  (format nil "XX~10:tYY")
  "XXYY")

;;; A pretty printing stream, but *print-pretty* is nil

(def-pprint-test format.\:t.2
  (with-output-to-string
   (s)
   (pprint-logical-block
    (s '(a b c))
    (format s "XX~10:tYY")))
  "XXYY"
  :pretty nil)

(def-pprint-test format.\:t.3
  (with-output-to-string
   (s)
   (pprint-logical-block
    (s '(a b c))
    (let ((*print-pretty* nil))
      (format s "XX~10:tYY"))))
  "XXYY")

;;; Positive tests

(def-pprint-test format.\:t.4
  (format nil "~<[~;~0,0:T~;]~:>" '(a))
  "[]")

(def-pprint-test format.\:t.5
  (format nil "~<[~;~1,0:T~;]~:>" '(a))
  "[ ]")

(def-pprint-test format.\:t.5a
  (format nil "~<[~;~,0:T~;]~:>" '(a))
  "[ ]")

(def-pprint-test format.\:t.6
  (format nil "~<[~;~0,1:T~;]~:>" '(a))
  "[ ]")

(def-pprint-test format.\:t.6a
  (format nil "~<[~;~0,:T~;]~:>" '(a))
  "[ ]")

(def-pprint-test format.\:t.6b
  (format nil "~<[~;~0:T~;]~:>" '(a))
  "[ ]")

(def-pprint-test format.\:t.7
  (loop for i from 0 to 20
	for s = (format nil "~<X~;~0,v:T~;Y~:>" (list i))
	unless (string= s (concatenate 'string "X" (make-string i :initial-element #\Space) "Y"))
	collect (list i s))
  nil)

(def-pprint-test format.\:t.8
  (loop for i from 0 to 20
	for s = (format nil "~<ABC~;~v,0:T~;DEF~:>" (list i))
	unless (string= s (concatenate 'string "ABC" (make-string i :initial-element #\Space) "DEF"))
	collect (list i s))
  nil)

(def-pprint-test format.\:t.9
  (loop
   for n0 = (random 10)
   for s0 = (make-string n0 :initial-element #\Space)
   for n1 = (random 30)
   for s1 = (make-string n1 :initial-element #\X)
   for n2 = (random 30)
   for inc = (random 20)
   for s2 = (cond
	     ((< n1 n2)
	      (concatenate 'string s0 s1 (make-string (- n2 n1)
						      :initial-element #\Space)))
	     ((= inc 0) (concatenate 'string s0 s1))
	     (t (loop do (incf n2 inc) while (<= n2 n1))
		(concatenate 'string s0 s1 (make-string (- n2 n1)
							:initial-element #\Space))))
   for result = (format nil (format nil "~A~~<~A~~~D,~D:T~~:>" s0 s1 n2 inc) '(a))
   repeat 100
   unless (string= s2 result)
   collect (list n0 n1 n2 inc s2 result))
  nil)

(def-pprint-test format.\:t.10
  (format nil "~<[~;~2,0:T~;]~:>" '(a))
  "[  ]")

(def-pprint-test format.\:t.11
  (format nil "~<[~;XXXX~2,0:T~;]~:>" '(a))
  "[XXXX]")

(def-pprint-test format.\:t.12
  (loop for n0 = (random 20)
	for s0 = (make-string n0 :initial-element #\Space)
	for n1 = (random 30)
	for s1 = (make-string n1 :initial-element #\X)
	for n2 = (random 30)
	for inc = (random 20)
	for s2 = (cond
		  ((< n1 n2)
		   (concatenate 'string s0 s1 (make-string (- n2 n1)
							   :initial-element #\Space)))
		  ((= inc 0) (concatenate 'string s0 s1))
		  (t (loop do (incf n2 inc) while (<= n2 n1))
		     (concatenate 'string s0 s1 (make-string (- n2 n1)
							     :initial-element #\Space))))
	for result = (format nil "~A~<~A~v,v:t~:>" s0 (list s1 n2 inc))
	repeat 100
	unless (string= s2 result)
	collect (list n1 n2 inc s2 result))
  nil)

;;; see 22.3.5.2

(deftest format.\:t.error.1
  (signals-error-always (format nil "~<XXX~1,1:TYYY~>") error)
  t t)

(deftest format.\:t.error.2
  (signals-error-always (format nil "~<XXX~:;YYY~>ZZZ~4,5:tWWW") error)
  t t)

(deftest format.\:t.error.3
  (signals-error-always (format nil "AAAA~1,1:TBBB~<XXX~:;YYY~>ZZZ") error)
  t t)

;;; ~:@t

(def-pprint-test format.\:@t.1
  (format nil "~<XXX~;~1,1:@t~;YYY~:>" '(a))
  "XXX YYY")

(def-pprint-test format.\:@t.1a
  (format nil "~<XXX~;~,1:@t~;YYY~:>" '(a))
  "XXX YYY")

(def-pprint-test format.\:@t.1b
  (format nil "~<XXX~;~1,:@t~;YYY~:>" '(a))
  "XXX YYY")

(def-pprint-test format.\:@t.1c
  (format nil "~<XXX~;~1:@t~;YYY~:>" '(a))
  "XXX YYY")

(def-pprint-test format.\:@t.1d
  (format nil "~<XXX~;~:@t~;YYY~:>" '(a))
  "XXX YYY")

(def-pprint-test format.\:@t.2
  (loop for colnum from 0 to 20
	for s1 = (format nil "~<XXXX~;~v,1:@t~:>" (list colnum))
	for s2 = (concatenate 'string "XXXX" (make-string colnum :initial-element #\Space))
	unless (string= s1 s2)
	collect (list colnum s1 s2))
  nil)

(def-pprint-test format.\:@t.3
  (loop for s0 = (make-string (random 20) :initial-element #\M)
	for colnum = (random 50)
	for colinc = (1+ (random 20))
	for s1 = (format nil "~A~<~v,v:@t~:>" s0 (list colnum colinc))
	for s2 = (concatenate 'string
			      s0
			      (make-string (* colinc (ceiling colnum colinc))
					   :initial-element #\Space))
	repeat 100
	unless (string= s1 s2)
	collect (list colnum colinc s1 s2))
  nil)

;;; Turned off if not pretty printing

(def-pprint-test format.\:@t.4
  (format nil "XX~10,20:@tYY")
  "XXYY"
  :pretty nil)

(def-pprint-test format.\:@t.5
  (with-output-to-string
   (s)
   (pprint-logical-block
    (s '(a b c))
    (format s "XX~10,20@:tYY")))
  "XXYY"
  :pretty nil)
