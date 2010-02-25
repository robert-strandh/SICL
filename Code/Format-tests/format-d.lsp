;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Jul 31 05:19:39 2004
;;;; Contains: Tests of the ~D format directive

(in-package :cl-test)

(compile-and-load "printer-aux.lsp")

(deftest format.d.1
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~D" i)
	 for j = (read-from-string s1)
	 repeat 1000
	 when (or (/= i j)
		  (find #\. s1)
		  (find #\+ s1)
		  (find-if #'alpha-char-p s1))
	 collect (list i s1 j)))
  nil)

(deftest formatter.d.1
  (let ((fn (formatter "~D")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for i = (- (random (+ x x)) x)
	   for s1 = (formatter-call-to-string fn i)
	   for j = (read-from-string s1)
	   repeat 1000
	   when (or (/= i j)
		    (find #\. s1)
		    (find #\+ s1)
		    (find-if #'alpha-char-p s1))
	   collect (list i s1 j))))
  nil)

(deftest format.d.2
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@d" i)
	 for j = (read-from-string s1)
	 repeat 1000
	 when (or (/= i j)
		  (find #\. s1)
		  ;; (find #\+ s1)
		  (find-if #'alpha-char-p s1))
	 collect (list i s1 j)))
  nil)

(deftest formatter.d.2
  (let ((fn (formatter "~@D")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for i = (- (random (+ x x)) x)
	   for s1 = (formatter-call-to-string fn i)
	   for j = (read-from-string s1)
	   repeat 1000
	   when (or (/= i j)
		    (find #\. s1)
		    ;; (find #\+ s1)
		    (find-if #'alpha-char-p s1))
	   collect (list i s1 j))))
  nil)

(deftest format.d.3
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~d" i)
	 for s2 = (format nil (format nil "~~~dd" mincol) i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest formatter.d.3
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~d" i)
	 for format-string = (format nil "~~~dd" mincol)
	 ; for s2 = (format nil format-string i)
	 for fn = (eval `(formatter ,format-string))
	 for s2 = (formatter-call-to-string fn i)
	 for pos = (search s1 s2)
	 repeat 100
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.d.4
  (with-standard-io-syntax
   (loop with limit = 10
	 with count = 0
	 for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@D" i)
	 for format-string = (format nil "~~~d@d" mincol)
	 for s2 = (format nil format-string i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (>= i 0) (not (eql (elt s1 0) #\+)))
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (if (> (incf count) limit)
		     "Count limit exceeded"
		     (list i mincol s1 format-string s2 pos))
	 while (<= count limit)))
  nil)

(deftest formatter.d.4
  (with-standard-io-syntax
   (loop with limit = 10
	 with count = 0
	 for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~@D" i)
	 for format-string = (format nil "~~~d@d" mincol)
	 for fn = (eval `(formatter ,format-string))
	 for s2 = (formatter-call-to-string fn i)
	 for pos = (search s1 s2)
	 repeat 100
	 when (or (null pos)
		  (and (>= i 0) (not (eql (elt s1 0) #\+)))
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (not (eql (position #\Space s2 :test-not #'eql)
				     (- (length s2) (length s1)))))))
	 collect (if (> (incf count) limit)
		     "Count limit exceeded"
		   (list i mincol s1 s2 pos))
	 while (<= count limit)))
  nil)

(deftest format.d.5
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~d" i)
	 for s2 = (format nil (format nil "~~~d,'~cd" mincol padchar) i)
	 for pos = (search s1 s2)
	 repeat 1000
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (find padchar s2 :end (- (length s2) (length s1))
				 :test-not #'eql))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest formatter.d.5
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for mincol = (random 30)
	 for padchar = (random-from-seq +standard-chars+)
	 for i = (- (random (+ x x)) x)
	 for s1 = (format nil "~d" i)
	 for format-string = (format nil "~~~d,'~cd" mincol padchar)
	 for fn = (eval `(formatter ,format-string))
	 for s2 = (formatter-call-to-string fn i)
	 for pos = (search s1 s2)
	 repeat 100
	 when (or (null pos)
		  (and (> mincol (length s1))
		       (or (/= (length s2) mincol)
			   (find padchar s2 :end (- (length s2) (length s1))
				 :test-not #'eql))))
	 collect (list i mincol s1 s2 pos)))
  nil)

(deftest format.d.6
  (let ((fn (formatter "~v,vd")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for mincol = (random 30)
	   for padchar = (random-from-seq +standard-chars+)
	   for i = (- (random (+ x x)) x)
	   for s1 = (format nil "~d" i)
	   for s2 = (format nil "~v,vD" mincol padchar i)
	   for s3 = (formatter-call-to-string fn mincol padchar i)
	   for pos = (search s1 s2)
	   repeat 1000
	   when (or (null pos)
		    (not (string= s2 s3))
		    (and (> mincol (length s1))
			 (or (/= (length s2) mincol)
			     (find padchar s2 :end (- (length s2) (length s1))
				   :test-not #'eql))))
	   collect (list i mincol s1 s2 s3 pos))))
  nil)

(deftest format.d.7
  (let ((fn (formatter "~v,v@D")))
    (with-standard-io-syntax
     (loop with limit = 10
	   with count = 0
	   for x = (ash 1 (+ 2 (random 80)))
	   for mincol = (random 30)
	   for padchar = (random-from-seq +standard-chars+)
	   for i = (- (random (+ x x)) x)
	   for s1 = (format nil "~@d" i)
	   for s2 = (format nil "~v,v@d" mincol padchar i)
	   for s3 = (formatter-call-to-string fn mincol padchar i)
	   for pos = (search s1 s2)
	   repeat 1000
	   when (or (null pos)
		    (not (string= s2 s3))
		    (and (>= i 0) (not (eql (elt s1 0) #\+)))
		    (and (> mincol (length s1))
			 (or (/= (length s2) mincol)
			     (find padchar s2 :end (- (length s2) (length s1))
				   :test-not #'eql))))
	   collect (if (> (incf count) limit)
		       "Count limit exceeded"
		     (list i mincol s1 s2 s3 pos))
	   while (<= count limit))))
  nil)

;;; Comma tests

(deftest format.d.8
  (let ((fn1 (formatter "~d"))
	(fn2 (formatter "~:d")))
    (loop for i from -999 to 999
	  for s1 = (format nil "~d" i)
	  for s2 = (format nil "~:d" i)
	  for s3 = (formatter-call-to-string fn1 i)
	  for s4 = (formatter-call-to-string fn2 i)
	  unless (and (string= s1 s2) (string= s1 s3) (string= s1 s4))
	  collect (list i s1 s2 s3 s4)))
  nil)

(deftest format.d.9
  (let ((fn1 (formatter "~d"))
	(fn2 (formatter "~:d")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for i = (- (random (+ x x)) x)
	   for commachar = #\,
	   for s1 = (format nil "~d" i)
	   for s2 = (format nil "~:d" i)
	   for s3 = (formatter-call-to-string fn1 i)
	   for s4 = (formatter-call-to-string fn2 i)
	   repeat 1000
	   unless (and (string= s1 s3)
		       (string= s2 s4)
		       (string= s1 (remove commachar s2))
		       (not (eql (elt s2 0) commachar))
		       (or (>= i 0) (not (eql (elt s2 1) commachar)))
		       (let ((len (length s2))
			     (ci+1 4))
			 (loop for i from (if (< i 0) 2 1) below len
			       always (if (= (mod (- len i) ci+1) 0)
					  (eql (elt s2 i) commachar)
					(find (elt s2 i) "0123456789")))))
	   collect (list x i commachar s1 s2 s3 s4))))
  nil)

(deftest format.d.10
  (let ((fn (formatter "~,,v:d")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for i = (- (random (+ x x)) x)
	   for commachar = (random-from-seq +standard-chars+)
	   for s1 = (format nil "~d" i)
	   for s2 = (format nil "~,,v:d" commachar i)
	   for s3 = (formatter-call-to-string fn commachar i)
	   repeat 1000
	   unless (and
		   (string= s2 s3)
		   (eql (elt s1 0) (elt s2 0))
		   (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		   (let ((len (length s2))
			 (ci+1 4)
			 (j (if (< i 0) 1 0)))
		     (loop for i from (if (< i 0) 2 1) below len
			   always (if (= (mod (- len i) ci+1) 0)
				      (eql (elt s2 i) commachar)
				    (eql (elt s1 (incf j)) (elt s2 i))))))
	   collect (list x i commachar s1 s2 s3))))
  nil)

(deftest format.d.11
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for s1 = (format nil "~d" i)
	 for format-string = (format nil "~~,,'~c:d" commachar)
	 for s2 = (format nil format-string i)
	 repeat 1000
	 unless (and
		 (eql (elt s1 0) (elt s2 0))
		 (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		 (let ((len (length s2))
		      (ci+1 4)
		      (j (if (< i 0) 1 0)))
		  (loop for i from (if (< i 0) 2 1) below len
			always (if (= (mod (- len i) ci+1) 0)
				   (eql (elt s2 i) commachar)
				 (eql (elt s1 (incf j)) (elt s2 i))))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest formatter.d.11
  (with-standard-io-syntax
   (loop for x = (ash 1 (+ 2 (random 80)))
	 for i = (- (random (+ x x)) x)
	 for commachar = (random-from-seq +standard-chars+)
	 for s1 = (format nil "~d" i)
	 for format-string = (format nil "~~,,'~c:d" commachar)
	 for fn = (eval `(formatter ,format-string))
	 ; for s2 = (format nil format-string i)
	 for s2 = (formatter-call-to-string fn i)
	 repeat 100
	 unless (and
		 (eql (elt s1 0) (elt s2 0))
		 (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		 (let ((len (length s2))
		      (ci+1 4)
		      (j (if (< i 0) 1 0)))
		  (loop for i from (if (< i 0) 2 1) below len
			always (if (= (mod (- len i) ci+1) 0)
				   (eql (elt s2 i) commachar)
				 (eql (elt s1 (incf j)) (elt s2 i))))))
	 collect (list x i commachar s1 s2)))
  nil)

(deftest format.d.12
  (let ((fn (formatter "~,,v,v:d")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for i = (- (random (+ x x)) x)
	   for commachar = (random-from-seq +standard-chars+)
	   for commaint = (1+ (random 20))
	   for s1 = (format nil "~d" i)
	   for s2 = (format nil "~,,v,v:D" commachar commaint i)
	   for s3 = (formatter-call-to-string fn commachar commaint i)
	   repeat 1000
	   unless (and
		   (string= s2 s3)
		   (eql (elt s1 0) (elt s2 0))
		   (if (< i 0) (eql (elt s1 1) (elt s2 1)) t)
		   (let ((len (length s2))
			 (ci+1 (1+ commaint))
			 (j (if (< i 0) 1 0)))
		     (loop for i from (if (< i 0) 2 1) below len
			   always (if (= (mod (- len i) ci+1) 0)
				      (eql (elt s2 i) commachar)
				    (eql (elt s1 (incf j)) (elt s2 i))))))
	   collect (list x i commachar s1 s2 s3))))
  nil)

(deftest format.d.13
  (let ((fn (formatter "~,,v,v:@D")))
    (with-standard-io-syntax
     (loop for x = (ash 1 (+ 2 (random 80)))
	   for i = (- (random (+ x x)) x)
	   for commachar = (random-from-seq +standard-chars+)
	   for commaint = (1+ (random 20))
	   for s1 = (format nil "~@d" i)
	   for s2 = (format nil "~,,v,v:@d" commachar commaint i)
	   for s3 = (formatter-call-to-string fn commachar commaint i)
	   repeat 1000
	   unless (and
		   (eql (elt s1 0) (elt s2 0))
		   (eql (elt s1 1) (elt s2 1))
		   (let ((len (length s2))
			 (ci+1 (1+ commaint))
			 (j 1))
		     (loop for i from 2 below len
			   always (if (= (mod (- len i) ci+1) 0)
				      (eql (elt s2 i) commachar)
				    (eql (elt s1 (incf j)) (elt s2 i))))))
	   collect (list x i commachar s1 s2 s3))))
  nil)

;;; NIL arguments

(def-format-test format.d.14
  "~vD" (nil 100) "100")

(def-format-test format.d.15
  "~6,vD" (nil 100) "   100")

(def-format-test format.d.16
  "~,,v:d" (nil 12345) "12,345")

(def-format-test format.d.17
  "~,,'*,v:d" (nil 12345) "12*345")

;;; When the argument is not an integer, print as if using ~A and base 10

(deftest format.d.18
  (loop for x in *mini-universe*
	for s1 = (format nil "~d" x)
	for s2 = (format nil "~A" x)
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.d.19
  (loop for x in *mini-universe*
	for s1 = (format nil "~:d" x)
	for s2 = (format nil "~A" x)
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.d.20
  (loop for x in *mini-universe*
	for s1 = (format nil "~@d" x)
	for s2 = (format nil "~A" x)
	unless (or (integerp x) (string= s1 s2))
	collect (list x s1 s2))
  nil)

(deftest format.d.21
  (loop for x in *mini-universe*
	for s1 = (format nil "~A" x)
	for s2 = (format nil "~@:d" x)
        for s3 = (format nil "~A" x)
	unless (or (integerp x) (string= s1 s2) (not (string= s1 s3)))
	collect (list x s1 s2))
  nil)

;;; Must add tests for non-integers when the parameters
;;; are specified, but it's not clear what the meaning is.
;;; Does mincol apply to the ~A equivalent?  What about padchar?
;;; Are comma-char and comma-interval always ignored?

;;; # arguments

(deftest format.d.22
  (apply
   #'values
   (loop for i from 0 to 10
	 for args = (make-list i)
	 for s = (apply #'format nil "~#d" 12345 args)
	 collect s))
  "12345"
  "12345"
  "12345"
  "12345"
  "12345"
  " 12345"
  "  12345"
  "   12345"
  "    12345"
  "     12345"
  "      12345")

(deftest formatter.d.22
  (apply
   #'values
   (let ((fn (formatter "~#D")))
     (loop for i from 0 to 10
	   for args = (make-list i)
	   ; for s = (apply #'format nil "~#d" 12345 args)
	   for s = (with-output-to-string
		     (stream)
		     (assert (equal (apply fn stream 12345 args) args)))
	   collect s)))
  "12345"
  "12345"
  "12345"
  "12345"
  "12345"
  " 12345"
  "  12345"
  "   12345"
  "    12345"
  "     12345"
  "      12345")

(deftest format.d.23
  (apply
   #'values
   (let ((fn (formatter "~,,,#:D")))
     (loop for i from 0 to 10
	   for args = (make-list i)
	   for s = (apply #'format nil "~,,,#:d" 1234567890 args)
	   for s2 = (with-output-to-string
		      (stream)
		      (assert (equal (apply fn stream 1234567890 args) args)))
	   do (assert (string= s s2))
	   collect s)))
  "1,2,3,4,5,6,7,8,9,0"
  "12,34,56,78,90"
  "1,234,567,890"
  "12,3456,7890"
  "12345,67890"
  "1234,567890"
  "123,4567890"
  "12,34567890"
  "1,234567890"
  "1234567890"
  "1234567890")

(deftest format.d.24
  (apply
   #'values
   (let ((fn (formatter "~,,,#:@d")))
     (loop for i from 0 to 10
	   for args = (make-list i)
	   for s = (apply #'format nil "~,,,#@:D" 1234567890 args)
	   for s2 = (with-output-to-string
		      (stream)
		      (assert (equal (apply fn stream 1234567890 args) args)))
	   do (assert (string= s s2))
	   collect s)))
  "+1,2,3,4,5,6,7,8,9,0"
  "+12,34,56,78,90"
  "+1,234,567,890"
  "+12,3456,7890"
  "+12345,67890"
  "+1234,567890"
  "+123,4567890"
  "+12,34567890"
  "+1,234567890"
  "+1234567890"
  "+1234567890")

(def-format-test format.d.25
  "~+10d" (1234) "      1234")

(def-format-test format.d.26
  "~+10@d" (1234) "     +1234")

(def-format-test format.d.27
  "~-1d" (1234) "1234")

(def-format-test format.d.28
  "~-1000000000000000000d" (1234) "1234")

(def-format-test format.d.29
  "~vd" ((1- most-negative-fixnum) 1234) "1234")

;;; Randomized test

(deftest format.d.30
  (let ((fn (formatter "~v,v,v,vD")))
    (loop
     for mincol = (and (coin) (random 50))
     for padchar = (and (coin)
			(random-from-seq +standard-chars+))
     for commachar = (and (coin)
			  (random-from-seq +standard-chars+))
     for commaint = (and (coin) (1+ (random 10)))
     for k = (ash 1 (+ 2 (random 30)))
     for x = (- (random (+ k k)) k)
     for fmt = (concatenate
		'string
		(if mincol (format nil "~~~d," mincol) "~,")
		(if padchar (format nil "'~c," padchar) ",")
		(if commachar (format nil "'~c," commachar) ",")
		(if commaint (format nil "~dd" commaint) "d"))
     for s1 = (format nil fmt x)
     for s2 = (format nil "~v,v,v,vd" mincol padchar commachar commaint x)
     for s3 = (formatter-call-to-string fn mincol padchar commachar commaint x)
     repeat 2000
     unless (and (string= s1 s2) (string= s2 s3))
     collect (list mincol padchar commachar commaint fmt x s1 s2 s3)))
  nil)
