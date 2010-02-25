;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jul 28 00:33:02 2004
;;;; Contains: Tests of the format directive ~R

(in-package :cl-test)

;;; Test of various radixes
(compile-and-load "printer-aux.lsp")
(compile-and-load "roman-numerals.lsp")

(deftest format.r.1
  (loop
   for i from 2 to 36
   for s = (format nil "~~~dR" i)
   nconc
   (loop for x = (let ((bound (ash 1 (+ 2 (random 40)))))
		   (- (random (* bound 2)) bound))
	 for s1 = (format nil s x)
	 for s2 = (with-standard-io-syntax
		   (write-to-string x :base i :readably nil))
	 repeat 100
	 unless (string= s1 s2)
	 collect (list i x s1 s2)))
  nil)

(deftest formatter.r.1
  (loop
   for i from 2 to 36
   for s = (format nil "~~~dR" i)
   for fn = (eval `(formatter ,s))
   nconc
   (loop for x = (let ((bound (ash 1 (+ 2 (random 40)))))
		   (- (random (* bound 2)) bound))
	 for s1 = (formatter-call-to-string fn x)
	 for s2 = (with-standard-io-syntax
		   (write-to-string x :base i :readably nil))
	 repeat 100
	 unless (string= s1 s2)
	 collect (list i x s1 s2)))
  nil)

(def-format-test format.r.2
  "~2r" (14) "1110")

(def-format-test format.r.3
  "~3r" (29) "1002")

(deftest format.r.4
  (loop for base from 2 to 36
	nconc
	(loop for mincol from 0 to 20
	      for fmt = (format nil "~~~D,~DR" base mincol)
	      for s = (format nil fmt base)
	      unless (if (<= mincol 2)
			 (string= s "10")
		       (string= (concatenate
				 'string
				 (make-string (- mincol 2)
					      :initial-element #\Space)
				 "10")
				s))
	      collect (list base mincol s)))
  nil)

(deftest formatter.r.4
  (loop for base from 2 to 36
	nconc
	(loop for mincol from 0 to 20
	      for fmt = (format nil "~~~D,~DR" base mincol)
	      for fn = (eval `(formatter ,fmt))
	      for s = (formatter-call-to-string fn base)
	      unless (if (<= mincol 2)
			 (string= s "10")
		       (string= (concatenate
				 'string
				 (make-string (- mincol 2)
					      :initial-element #\Space)
				 "10")
				s))
	      collect (list base mincol s)))
  nil)

(deftest format.r.5
  (loop for base from 2 to 36
	nconc
	(loop for mincol from 0 to 20
	      for fmt = (format nil "~~~D,~D,'*r" base mincol)
	      for s = (format nil fmt base)
	      unless (if (<= mincol 2)
			 (string= s "10")
		       (string= (concatenate
				 'string
				 (make-string (- mincol 2)
					      :initial-element #\*)
				 "10")
				s))
	      collect (list base mincol s)))
  nil)

(deftest formatter.r.5
  (loop for base from 2 to 36
	nconc
	(loop for mincol from 0 to 20
	      for fmt = (format nil "~~~D,~D,'*r" base mincol)
	      for fn = (eval `(formatter ,fmt))
	      for s = (formatter-call-to-string fn base)
	      unless (if (<= mincol 2)
			 (string= s "10")
		       (string= (concatenate
				 'string
				 (make-string (- mincol 2)
					      :initial-element #\*)
				 "10")
				s))
	      collect (list base mincol s)))
  nil)

(deftest format.r.6
  (loop for base from 2 to 36
	for s = (format nil "~vr" base (1+ base))
	unless (string= s "11")
	collect (list base s))
  nil)

(deftest formatter.r.6
  (let ((fn (formatter "~vr")))
    (loop for base from 2 to 36
	  for s = (formatter-call-to-string fn base (1+ base))
	  unless (string= s "11")
	  collect (list base s)))
  nil)

(defparameter *english-number-names*
  '("zero"
   "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"
   "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
   "seventeen" "eighteen" "nineteen" "twenty"
   "twenty-one" "twenty-two" "twenty-three" "twenty-four" "twenty-five"
   "twenty-six" "twenty-seven" "twenty-eight" "twenty-nine" "thirty"
   "thirty-one" "thirty-two" "thirty-three" "thirty-four" "thirty-five"
   "thirty-six" "thirty-seven" "thirty-eight" "thirty-nine" "forty"
   "forty-one" "forty-two" "forty-three" "forty-four" "forty-five"
   "forty-six" "forty-seven" "forty-eight" "forty-nine" "fifty"
   "fifty-one" "fifty-two" "fifty-three" "fifty-four" "fifty-five"
   "fifty-six" "fifty-seven" "fifty-eight" "fifty-nine" "sixty"
   "sixty-one" "sixty-two" "sixty-three" "sixty-four" "sixty-five"
   "sixty-six" "sixty-seven" "sixty-eight" "sixty-nine" "seventy"
   "seventy-one" "seventy-two" "seventy-three" "seventy-four" "seventy-five"
   "seventy-six" "seventy-seven" "seventy-eight" "seventy-nine" "eighty"
   "eighty-one" "eighty-two" "eighty-three" "eighty-four" "eighty-five"
   "eighty-six" "eighty-seven" "eighty-eight" "eighty-nine" "ninety"
   "ninety-one" "ninety-two" "ninety-three" "ninety-four" "ninety-five"
   "ninety-six" "ninety-seven" "ninety-eight" "ninety-nine" "one hundred"))

(deftest format.r.7
  (loop for i from 0 to 100
	for s1 = (format nil "~r" i)
	for s2 in *english-number-names*
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(deftest formatter.r.7
  (let ((fn (formatter "~r")))
    (loop for i from 0 to 100
	  for s1 = (formatter-call-to-string fn i)
	  for s2 in *english-number-names*
	  unless (string= s1 s2)
	  collect (list i s1 s2)))
  nil)

(deftest format.r.7a
  (loop for i from 1 to 100
	for s1 = (format nil "~r" (- i))
	for s2 in (cdr *english-number-names*)
	for s3 = (concatenate 'string "negative " s2)
	for s4 = (concatenate 'string "minus " s2)
	unless (or (string= s1 s3) (string= s1 s4))
	collect (list i s1 s3 s4))
  nil)

(def-format-test format.r.8
  "~vr" (nil 5) "five")

(def-format-test format.r.9
  "~#r" (4 nil nil) "11" 2)

(deftest format.r.10
  (with-standard-io-syntax
   (let ((*print-radix* t))
     (format nil "~10r" 123)))
  "123")

(deftest formatter.r.10
  (let ((fn (formatter "~10r")))
    (with-standard-io-syntax
     (let ((*print-radix* t))
       (values
	(format nil fn 123)
	(formatter-call-to-string fn 123)))))
  "123"
  "123")

(def-format-test format.r.11
  "~8@R" (65) "+101")

(def-format-test format.r.12
  "~2:r" (126) "1,111,110")

(def-format-test format.r.13
  "~3@:r" (#3r2120012102) "+2,120,012,102")

(deftest format.r.14
  (loop
   for i from 2 to 36
   for s = (format nil "~~~d:R" i)
   nconc
   (loop for x = (let ((bound (ash 1 (+ 2 (random 40)))))
		   (- (random (* bound 2)) bound))
	 for s1 = (remove #\, (format nil s x))
	 for y = (let ((*read-base* i)) (read-from-string s1))
	 repeat 100
	 unless (= x y)
	 collect (list i x s1 y)))
  nil)

(deftest format.r.15
  (loop
   for i = (+ 2 (random 35))
   for interval = (1+ (random 20))
   for comma = (loop for c = (random-from-seq +standard-chars+)
		     unless (alphanumericp c)
		     return c)
   for s = (format nil "~~~d,,,'~c,~d:R" i comma interval)
   for x = (let ((bound (ash 1 (+ 2 (random 40)))))
	     (- (random (* bound 2)) bound))
   for s1 = (remove comma (format nil s x))
   for y = (let ((*read-base* i)) (read-from-string s1))
   repeat 1000
   unless (or (and (eql comma #\-) (< x 0))
	      (= x y))
   collect (list i interval comma x s1 y))
  nil)

(def-format-test format.r.16
  "~2,,,,1000000000000000000r" (17) "10001")

(def-format-test format.r.17
  "~8,10:@r" (#o526104) "  +526,104")

(defparameter *english-ordinal-names*
  '("zeroth"
   "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth"
   "eleventh" "twelfth" "thirteenth" "fourteenth" "fifteenth" "sixteenth"
   "seventeenth" "eighteenth" "nineteenth" "twentieth"
   "twenty-first" "twenty-second" "twenty-third" "twenty-fourth" "twenty-fifth"
   "twenty-sixth" "twenty-seventh" "twenty-eighth" "twenty-ninth" "thirtieth"
   "thirty-first" "thirty-second" "thirty-third" "thirty-fourth" "thirty-fifth"
   "thirty-sixth" "thirty-seventh" "thirty-eighth" "thirty-ninth" "fortieth"
   "forty-first" "forty-second" "forty-third" "forty-fourth" "forty-fifth"
   "forty-sixth" "forty-seventh" "forty-eighth" "forty-ninth" "fiftieth"
   "fifty-first" "fifty-second" "fifty-third" "fifty-fourth" "fifty-fifth"
   "fifty-sixth" "fifty-seventh" "fifty-eighth" "fifty-ninth" "sixtieth"
   "sixty-first" "sixty-second" "sixty-third" "sixty-fourth" "sixty-fifth"
   "sixty-sixth" "sixty-seventh" "sixty-eighth" "sixty-ninth" "seventieth"
   "seventy-first" "seventy-second" "seventy-third" "seventy-fourth" "seventy-fifth"
   "seventy-sixth" "seventy-seventh" "seventy-eighth" "seventy-ninth" "eightieth"
   "eighty-first" "eighty-second" "eighty-third" "eighty-fourth" "eighty-fifth"
   "eighty-sixth" "eighty-seventh" "eighty-eighth" "eighty-ninth" "ninetieth"
   "ninety-first" "ninety-second" "ninety-third" "ninety-fourth" "ninety-fifth"
   "ninety-sixth" "ninety-seventh" "ninety-eighth" "ninety-ninth" "one hundredth"))

(deftest format.r.18
  (loop for i from 0 to 100
	for s1 = (format nil "~:r" i)
	for s2 in *english-ordinal-names*
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(deftest formatter.r.18
  (let ((fn (formatter "~:r")))
    (loop for i from 0 to 100
	  for s1 = (formatter-call-to-string fn i)
	  for s2 in *english-ordinal-names*
	  unless (string= s1 s2)
	  collect (list i s1 s2)))
  nil)

(deftest format.r.18a
  (loop for i from 1 to 100
	for s1 = (format nil "~:r" (- i))
	for s2 in (cdr *english-ordinal-names*)
	for s3 = (concatenate 'string "negative " s2)
	for s4 = (concatenate 'string "minus " s2)
	unless (or (string= s1 s3) (string= s1 s4))
	collect (list i s1 s3 s4))
  nil)

(deftest format.r.19
  (loop for i from 1
	for s1 in *roman-numerals*
	for s2 = (format nil "~@R" i)
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(deftest formatter.r.19
  (let ((fn (formatter "~@r")))
    (loop for i from 1
	  for s1 in *roman-numerals*
	  for s2 = (formatter-call-to-string fn i)
	  unless (string= s1 s2)
	  collect (list i s1 s2)))
  nil)

;;; Old roman numerals

(defun old-roman-numeral (x)
  (assert (typep x '(integer 1)))
  (let ((n-m 0)
	(n-d 0)
	(n-c 0)
	(n-l 0)
	(n-x 0)
	(n-v 0)
	)
    (loop while (>= x 1000) do (incf n-m) (decf x 1000))
    (when (>= x 500) (incf n-d) (decf x 500))
    (loop while (>= x 100) do (incf n-c) (decf x 100))
    (when (>= x 50) (incf n-l) (decf x 50))
    (loop while (>= x 10) do (incf n-x) (decf x 10))
    (when (>= x 5) (incf n-v) (decf x 5))
    (concatenate 'string
		 (make-string n-m :initial-element #\M)
		 (make-string n-d :initial-element #\D)
		 (make-string n-c :initial-element #\C)
		 (make-string n-l :initial-element #\L)
		 (make-string n-x :initial-element #\X)
		 (make-string n-v :initial-element #\V)
		 (make-string x   :initial-element #\I))))

(deftest format.r.20
  (loop for i from 1 to 4999
	for s1 = (format nil "~:@r" i)
	for s2 = (old-roman-numeral i)
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

(deftest formatter.r.20
  (let ((fn (formatter "~@:R")))
    (loop for i from 1 to 4999
	  for s1 = (formatter-call-to-string fn i)
	  for s2 = (old-roman-numeral i)
	  unless (string= s1 s2)
	  collect (list i s1 s2)))
  nil)

(deftest format.r.21
  (loop for i from 1 to 4999
	for s1 = (format nil "~:@r" i)
	for s2 = (format nil "~@:R" i)
	unless (string= s1 s2)
	collect (list i s1 s2))
  nil)

;; Combinations of mincol and comma chars

(def-format-test format.r.22
  "~2,12,,'*:r" (#b1011101) "   1*011*101")

(def-format-test format.r.23
  "~3,14,'X,',:R" (#3r1021101) "XXXXX1,021,101")

;; v directive in various positions

(def-format-test format.r.24
  "~10,vr" (nil 12345) "12345")

(deftest format.r.25
  (loop for i from 0 to 5
	for s = (format nil "~10,vr" i 12345)
	unless (string= s "12345")
	collect (list i s))
  nil)

(deftest formatter.r.25
  (let ((fn (formatter "~10,vr")))
    (loop for i from 0 to 5
	  for s = (formatter-call-to-string fn i 12345)
	  unless (string= s "12345")
	  collect (list i s)))
  nil)

(def-format-test format.r.26
  "~10,#r" (12345 nil nil nil nil nil) " 12345" 5)

(def-format-test format.r.27
  "~10,12,vr" (#\/ 123456789) "///123456789")

(def-format-test format.r.28
  "~10,,,v:r" (#\/ 123456789) "123/456/789")

(def-format-test format.r.29
  "~10,,,v:r" (nil 123456789) "123,456,789")

(def-format-test format.r.30
  "~8,,,,v:R" (nil #o12345670) "12,345,670")

(def-format-test format.r.31
  "~8,,,,v:R" (2 #o12345670) "12,34,56,70")

(def-format-test format.r.32
  "~16,,,,#:r" (#x12345670 nil nil nil) "1234,5670" 3)

(def-format-test format.r.33
  "~16,,,,1:r" (#x12345670) "1,2,3,4,5,6,7,0")

;;; Explicit signs

(def-format-test format.r.34
  "~+10r" (12345) "12345")

(def-format-test format.r.35
  "~10,+8r" (12345) "   12345")

(def-format-test format.r.36
  "~10,0r" (12345) "12345")

(def-format-test format.r.37
  "~10,-1r" (12345) "12345")

(def-format-test format.r.38
  "~10,-1000000000000000r" (12345) "12345")

;;; Randomized test

(deftest format.r.39
  (let ((fn (formatter "~v,v,v,v,vr")))
    (loop
     for radix = (+ 2 (random 35))
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
		(format nil "~~~d," radix)
		(if mincol (format nil "~d," mincol) ",")
		(if padchar (format nil "'~c," padchar) ",")
		(if commachar (format nil "'~c," commachar) ",")
		(if commaint (format nil "~dr" commaint) "r"))
     for s1 = (format nil fmt x)
     for s2 = (format nil "~v,v,v,v,vr" radix mincol padchar commachar commaint x)
     for s3 = (formatter-call-to-string fn radix mincol padchar commachar commaint x)
     repeat 2000
     unless (and (string= s1 s2)
		 (string= s1 s3))
     collect (list radix mincol padchar commachar commaint fmt x s1 s2 s3)))
  nil)
