(cl:in-package #:sicl-format-test)

(define-test general
  ;; multiple occurrences of a modifier
  ;; should signal an error.
  (assert-error 'error (format nil "~::d" 0))
  (assert-error 'error (format nil "~@@d" 0))
  (assert-error 'error (format nil "~@:@d" 0))
  (assert-error 'error (format nil "~:@:d" 0)))

;;; When the second argument to format is a constant
;;; string, a compiler macro replaces the call
;;; to format with a compiled version of the call.
;;; To test the interpreter, we must therefore make
;;; sure that the second argument is not a constant
;;; string.  Do that by replacing the constant string
;;; "..." by (progn "...").
(defmacro expand-format (expression)
  `(progn ,expression
	  ,(mapcar (lambda (subexpr)
		     (if (and (consp subexpr)
			      (eq (car subexpr) 'format))
			 `(format ,(cadr subexpr)
				  (progn ,(caddr subexpr))
				  ,@(cdddr subexpr))
			 subexpr))
		   expression)))

(define-test nesting
  (expand-format
   (assert-equal "ABCD"
		 (format nil "~{~a~}~{~a~}" '(a b) '(c d)))))

(define-test character
  ;; without a : modifier, do what write-char does
  (loop for code from 0 to 1000
	for char = (code-char code)
	do (expand-format (assert-equal
			   (with-output-to-string (stream)
			     (write-char char stream))
			   (format nil "~c" char))))
  ;; with the : modifier, if it is a printing
  ;; character, do what write-char does, and if
  ;; not do what char-name does
  (loop for code from 0 to 1000
	for char = (code-char code)
	do (if (and (graphic-char-p char)
		    (not (eql char #\Space)))
	       (expand-format (assert-equal
			       (with-output-to-string (stream)
				 (write-char char stream))
			       (format nil "~:c" char)))
	       (expand-format (assert-equal
			       (char-name char)
			       (format nil "~:c" char)))))
  ;; with the @ modifier, do what prin1 does
  (loop for code from 0 to 1000
	for char = (code-char code)
	do (expand-format (assert-equal
			   (prin1-to-string char)
			   (format nil "~@c" char))))
  ;; using ~c with something other than a
  ;; character should signal an error
  (loop for thing in '(1 "string" 'symbol *standard-output*)
	do (expand-format (assert-error 'error (format nil "~c" thing))))
  ;; the character directive does not take any parameters
  (expand-format (assert-error 'error (format nil "~2c" #\a)))
  (expand-format (assert-error 'error (format nil "~'2c" #\a)))
  (expand-format (assert-error 'error (format nil "~#c" #\a)))
  (expand-format (assert-error 'error (format nil "~vc" #\a))))

(define-test newline
  ;; without any parameters, output a newline
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Newline stream))
		  (format nil "~%")))
  ;; also with a pameter of 1
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Newline stream))
		  (format nil "~1%")))
  (expand-format (assert-equal
		  ""
		  (format nil "~0%")))
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Newline stream)
		    (write-char #\Newline stream))
		  (format nil "~2%")))
  ;; The newline directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (format nil "~'a%"))
  (assert-error 'error (format nil "~1,2%"))
  ;; The newline directive takes no modifiers
  (assert-error 'error (format nil "~:%"))
  (assert-error 'error (format nil "~@%")))

(define-test fresh-line
  ;; without any parameters, does nothing to a string
  (expand-format (assert-equal
		  ""
		  (format nil "~&")))
  ;; same thing for parameter values of 0 and 1
  (expand-format (assert-equal
		  ""
		  (format nil "~0&")))
  (expand-format (assert-equal
		  ""
		  (format nil "~1&")))
  ;; for a parameter value of 2, outputs a newline
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Newline stream))
		  (format nil "~2&")))
  ;; The fresh-line directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (format nil "~'a&"))
  (assert-error 'error (format nil "~1,2&"))
  ;; The fresh-line directive takes no modifiers
  (assert-error 'error (format nil "~:&"))
  (assert-error 'error (format nil "~@&")))
    
(define-test page
  ;; without any parameters, outputs a page separator
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Page stream))
		  (format nil "~|")))
  ;; same thing for a parameter value of 1
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Page stream))
		  (format nil "~1|")))
  ;; with a parameter value of 0, does nothing
  (expand-format (assert-equal
		  ""
		  (format nil "~0|")))
  ;; for a parameter value of 2, outputs two page separators
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\Page stream)
		    (write-char #\Page stream))
		  (format nil "~2|")))
  ;; The page directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (format nil "~'a|"))
  (assert-error 'error (format nil "~1,2|"))
  ;; The page directive takes no modifiers
  (assert-error 'error (format nil "~:|"))
  (assert-error 'error (format nil "~@|")))
    
(define-test tilde
  ;; without any parameters, outputs a tilde
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\~ stream))
		  (format nil "~~")))
  ;; same thing for a parameter value of 1
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\~ stream))
		  (format nil "~1~")))
  ;; with a parameter value of 0, does nothing
  (expand-format (assert-equal
		  ""
		  (format nil "~0~")))
  ;; for a parameter value of 2, outputs two tildes
  (expand-format (assert-equal
		  (with-output-to-string (stream)
		    (write-char #\~ stream)
		    (write-char #\~ stream))
		  (format nil "~2~")))
  ;; The tilde directive takes a single optional
  ;; numeric parameter
  (assert-error 'error (format nil "~'a~"))
  (assert-error 'error (format nil "~1,2~"))
  ;; The tilde directive takes no modifiers
  (assert-error 'error (format nil "~:~"))
  (assert-error 'error (format nil "~@~")))
    
(define-test radix
  ;; English cardinal numbers
  (expand-format (assert-equal "zero" (format nil "~r" 0)))
  (expand-format (assert-equal "one" (format nil "~r" 1)))
  (expand-format (assert-equal "two" (format nil "~r" 2)))
  (expand-format (assert-equal "three" (format nil "~r" 3)))
  (expand-format (assert-equal "four" (format nil "~r" 4)))
  (expand-format (assert-equal "five" (format nil "~r" 5)))
  (expand-format (assert-equal "six" (format nil "~r" 6)))
  (expand-format (assert-equal "seven" (format nil "~r" 7)))
  (expand-format (assert-equal "eight" (format nil "~r" 8)))
  (expand-format (assert-equal "nine" (format nil "~r" 9)))
  (expand-format (assert-equal "ten" (format nil "~r" 10)))
  (expand-format (assert-equal "eleven" (format nil "~r" 11)))
  (expand-format (assert-equal "twelve" (format nil "~r" 12)))
  (expand-format (assert-equal "thirteen" (format nil "~r" 13)))
  (expand-format (assert-equal "fourteen" (format nil "~r" 14)))
  (expand-format (assert-equal "fifteen" (format nil "~r" 15)))
  (expand-format (assert-equal "sixteen" (format nil "~r" 16)))
  (expand-format (assert-equal "seventeen" (format nil "~r" 17)))
  (expand-format (assert-equal "eighteen" (format nil "~r" 18)))
  (expand-format (assert-equal "nineteen" (format nil "~r" 19)))
  (expand-format (assert-equal "twenty" (format nil "~r" 20)))
  (expand-format (assert-equal "twenty-one" (format nil "~r" 21)))
  (expand-format (assert-equal "thirty" (format nil "~r" 30)))
  (expand-format (assert-equal "fourty" (format nil "~r" 40)))
  (expand-format (assert-equal "fifty" (format nil "~r" 50)))
  (expand-format (assert-equal "sixty" (format nil "~r" 60)))
  (expand-format (assert-equal "seventy" (format nil "~r" 70)))
  (expand-format (assert-equal "eighty" (format nil "~r" 80)))
  (expand-format (assert-equal "ninety" (format nil "~r" 90)))
  (expand-format (assert-equal "one hundred" (format nil "~r" 100)))
  (expand-format (assert-equal "two hundred four" (format nil "~r" 204)))
  (expand-format (assert-equal "three hundred sixteen" (format nil "~r" 316)))
  (expand-format (assert-equal "four hundred thirty-six" (format nil "~r" 436)))
  (expand-format (assert-equal "two thousand" (format nil "~r" 2000)))
  (expand-format (assert-equal "three thousand five" (format nil "~r" 3005)))
  (expand-format (assert-equal "four thousand twelve" (format nil "~r" 4012)))
  (expand-format (assert-equal "five thousand two hundred" (format nil "~r" 5200)))
  (expand-format (assert-equal "eighty thousand" (format nil "~r" 80000)))
  (expand-format (assert-equal "five hundred thousand" (format nil "~r" 500000)))
  (expand-format (assert-equal "two million" (format nil "~r" 2000000)))
  (expand-format (assert-equal "three million six" (format nil "~r" 3000006)))
  (expand-format (assert-equal "four million two thousand" (format nil "~r" 4002000)))
  ;; English ordinal numbers
  (expand-format (assert-equal "zeroth" (format nil "~:r" 0)))
  (expand-format (assert-equal "first" (format nil "~:r" 1)))
  (expand-format (assert-equal "second" (format nil "~:r" 2)))
  (expand-format (assert-equal "third" (format nil "~:r" 3)))
  (expand-format (assert-equal "fourth" (format nil "~:r" 4)))
  (expand-format (assert-equal "fifth" (format nil "~:r" 5)))
  (expand-format (assert-equal "sixth" (format nil "~:r" 6)))
  (expand-format (assert-equal "seventh" (format nil "~:r" 7)))
  (expand-format (assert-equal "eighth" (format nil "~:r" 8)))
  (expand-format (assert-equal "ninth" (format nil "~:r" 9)))
  (expand-format (assert-equal "tenth" (format nil "~:r" 10)))
  (expand-format (assert-equal "eleventh" (format nil "~:r" 11)))
  (expand-format (assert-equal "twelvth" (format nil "~:r" 12)))
  (expand-format (assert-equal "thirteenth" (format nil "~:r" 13)))
  (expand-format (assert-equal "fourteenth" (format nil "~:r" 14)))
  (expand-format (assert-equal "fifteenth" (format nil "~:r" 15)))
  (expand-format (assert-equal "sixteenth" (format nil "~:r" 16)))
  (expand-format (assert-equal "seventeenth" (format nil "~:r" 17)))
  (expand-format (assert-equal "eighteenth" (format nil "~:r" 18)))
  (expand-format (assert-equal "nineteenth" (format nil "~:r" 19)))
  (expand-format (assert-equal "twentieth" (format nil "~:r" 20)))
  (expand-format (assert-equal "twenty-first" (format nil "~:r" 21)))
  (expand-format (assert-equal "thirtieth" (format nil "~:r" 30)))
  (expand-format (assert-equal "fourtieth" (format nil "~:r" 40)))
  (expand-format (assert-equal "fiftieth" (format nil "~:r" 50)))
  (expand-format (assert-equal "sixtieth" (format nil "~:r" 60)))
  (expand-format (assert-equal "seventieth" (format nil "~:r" 70)))
  (expand-format (assert-equal "eightieth" (format nil "~:r" 80)))
  (expand-format (assert-equal "ninetieth" (format nil "~:r" 90)))
  (expand-format (assert-equal "one hundredth" (format nil "~:r" 100)))
  (expand-format (assert-equal "two hundred fourth" (format nil "~:r" 204)))
  (expand-format (assert-equal "three hundred sixteenth" (format nil "~:r" 316)))
  (expand-format (assert-equal "four hundred thirty-sixth" (format nil "~:r" 436)))
  (expand-format (assert-equal "two thousandth" (format nil "~:r" 2000)))
  (expand-format (assert-equal "three thousand fifth" (format nil "~:r" 3005)))
  (expand-format (assert-equal "four thousand twelvth" (format nil "~:r" 4012)))
  (expand-format (assert-equal "five thousand two hundredth" (format nil "~:r" 5200)))
  (expand-format (assert-equal "eighty thousandth" (format nil "~:r" 80000)))
  (expand-format (assert-equal "five hundred thousandth" (format nil "~:r" 500000)))
  (expand-format (assert-equal "two millionth" (format nil "~:r" 2000000)))
  (expand-format (assert-equal "three million sixth" (format nil "~:r" 3000006)))
  (expand-format (assert-equal "four million two thousandth" (format nil "~:r" 4002000)))
  ;; Roman numerals
  (expand-format (assert-equal "I" (format nil "~@r" 1)))
  (expand-format (assert-equal "II" (format nil "~@r" 2)))
  (expand-format (assert-equal "III" (format nil "~@r" 3)))
  (expand-format (assert-equal "IV" (format nil "~@r" 4)))
  (expand-format (assert-equal "V" (format nil "~@r" 5)))
  (expand-format (assert-equal "VI" (format nil "~@r" 6)))
  (expand-format (assert-equal "VII" (format nil "~@r" 7)))
  (expand-format (assert-equal "VIII" (format nil "~@r" 8)))
  (expand-format (assert-equal "IX" (format nil "~@r" 9)))
  (expand-format (assert-equal "X" (format nil "~@r" 10)))
  (expand-format (assert-equal "XI" (format nil "~@r" 11)))
  (expand-format (assert-equal "XII" (format nil "~@r" 12)))
  (expand-format (assert-equal "XIII" (format nil "~@r" 13)))
  (expand-format (assert-equal "XIV" (format nil "~@r" 14)))
  (expand-format (assert-equal "XV" (format nil "~@r" 15)))
  (expand-format (assert-equal "XVI" (format nil "~@r" 16)))
  (expand-format (assert-equal "XVII" (format nil "~@r" 17)))
  (expand-format (assert-equal "XVIII" (format nil "~@r" 18)))
  (expand-format (assert-equal "XIX" (format nil "~@r" 19)))
  (expand-format (assert-equal "XX" (format nil "~@r" 20)))
  (expand-format (assert-equal "XXX" (format nil "~@r" 30)))
  (expand-format (assert-equal "XL" (format nil "~@r" 40)))
  (expand-format (assert-equal "L" (format nil "~@r" 50)))
  (expand-format (assert-equal "LXIV" (format nil "~@r" 64)))
  (expand-format (assert-equal "XCIX" (format nil "~@r" 99)))
  (expand-format (assert-equal "C" (format nil "~@r" 100)))
  (expand-format (assert-equal "CXLVII" (format nil "~@r" 147)))
  (expand-format (assert-equal "CDLXXXIX" (format nil "~@r" 489)))
  (expand-format (assert-equal "DCCCXXXI" (format nil "~@r" 831)))
  (expand-format (assert-equal "M" (format nil "~@r" 1000)))
  (expand-format (assert-equal "MMXL" (format nil "~@r" 2040)))
  (expand-format (assert-equal "MMMXC" (format nil "~@r" 3090)))
  ;; Old Roman numerals
  (expand-format (assert-equal "I" (format nil "~:@r" 1)))
  (expand-format (assert-equal "II" (format nil "~:@r" 2)))
  (expand-format (assert-equal "III" (format nil "~:@r" 3)))
  (expand-format (assert-equal "IIII" (format nil "~:@r" 4)))
  (expand-format (assert-equal "V" (format nil "~:@r" 5)))
  (expand-format (assert-equal "VI" (format nil "~:@r" 6)))
  (expand-format (assert-equal "VII" (format nil "~:@r" 7)))
  (expand-format (assert-equal "VIII" (format nil "~:@r" 8)))
  (expand-format (assert-equal "VIIII" (format nil "~:@r" 9)))
  (expand-format (assert-equal "X" (format nil "~:@r" 10)))
  (expand-format (assert-equal "XI" (format nil "~:@r" 11)))
  (expand-format (assert-equal "XII" (format nil "~:@r" 12)))
  (expand-format (assert-equal "XIII" (format nil "~:@r" 13)))
  (expand-format (assert-equal "XIIII" (format nil "~:@r" 14)))
  (expand-format (assert-equal "XV" (format nil "~:@r" 15)))
  (expand-format (assert-equal "XVI" (format nil "~:@r" 16)))
  (expand-format (assert-equal "XVII" (format nil "~:@r" 17)))
  (expand-format (assert-equal "XVIII" (format nil "~:@r" 18)))
  (expand-format (assert-equal "XVIIII" (format nil "~:@r" 19)))
  (expand-format (assert-equal "XX" (format nil "~:@r" 20)))
  (expand-format (assert-equal "XXX" (format nil "~:@r" 30)))
  (expand-format (assert-equal "XXXX" (format nil "~:@r" 40)))
  (expand-format (assert-equal "L" (format nil "~:@r" 50)))
  (expand-format (assert-equal "LXIIII" (format nil "~:@r" 64)))
  (expand-format (assert-equal "LXXXXVIIII" (format nil "~:@r" 99)))
  (expand-format (assert-equal "C" (format nil "~:@r" 100)))
  (expand-format (assert-equal "CXXXXVII" (format nil "~:@r" 147)))
  (expand-format (assert-equal "CCCCLXXXVIIII" (format nil "~:@r" 489)))
  (expand-format (assert-equal "DCCCXXXI" (format nil "~:@r" 831)))
  (expand-format (assert-equal "M" (format nil "~:@r" 1000)))
  (expand-format (assert-equal "MMXXXX" (format nil "~:@r" 2040)))
  (expand-format (assert-equal "MMMLXXXX" (format nil "~:@r" 3090)))
  ;; test the use of different values of the radix
  (loop for radix from 2 to 36
	do (loop repeat 1000
		 do (let ((value (random (expt 10 100))))
		      (expand-format
		       (assert-equal
			(with-output-to-string (stream)
			  (let ((*print-base* radix))
			    (princ value stream)))
			(format nil "~vr" radix value))))))
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
		 (format nil "~10,1r" 123)))
  (expand-format
   (assert-equal "123"
		 (format nil "~10,2r" 123)))
  (expand-format
   (assert-equal "123"
		 (format nil "~10,3r" 123)))
  (expand-format
   (assert-equal " 123"
		 (format nil "~10,4r" 123)))
  (expand-format
   (assert-equal "  123"
		 (format nil "~10,5r" 123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
		 (format nil "~10,5,'xr" 123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
		 (format nil "~10,5,'x:r" 123)))
   (expand-format
    (assert-equal "xx1,234"
 		 (format nil "~10,7,'x:r" 1234)))
   (expand-format
    (assert-equal "xx551,234"
 		 (format nil "~10,9,'x:r" 551234)))
   (expand-format
    (assert-equal "xx66,551,234"
 		 (format nil "~10,12,'x:r" 66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
 		 (format nil "~10,12,'x,'a:r" 66551234)))

  )

(define-test decimal
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
		 (format nil "~1d" 123)))
  (expand-format
   (assert-equal "123"
		 (format nil "~2d" 123)))
  (expand-format
   (assert-equal "123"
		 (format nil "~3d" 123)))
  (expand-format
   (assert-equal " 123"
		 (format nil "~4d" 123)))
  (expand-format
   (assert-equal "  123"
		 (format nil "~5d" 123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
		 (format nil "~5,'xd" 123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
		 (format nil "~5,'x:d" 123)))
   (expand-format
    (assert-equal "xx1,234"
 		 (format nil "~7,'x:d" 1234)))
   (expand-format
    (assert-equal "xx551,234"
 		 (format nil "~9,'x:d" 551234)))
   (expand-format
    (assert-equal "xx66,551,234"
 		 (format nil "~12,'x:d" 66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
 		 (format nil "~12,'x,'a:d" 66551234)))

  )

(define-test octal
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "123"
		 (format nil "~1o" #o123)))
  (expand-format
   (assert-equal "123"
		 (format nil "~2o" #o123)))
  (expand-format
   (assert-equal "123"
		 (format nil "~3o" #o123)))
  (expand-format
   (assert-equal " 123"
		 (format nil "~4o" #o123)))
  (expand-format
   (assert-equal "  123"
		 (format nil "~5o" #o123)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx123"
		 (format nil "~5,'xo" #o123)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx123"
		 (format nil "~5,'x:o" #o123)))
   (expand-format
    (assert-equal "xx1,234"
 		 (format nil "~7,'x:o" #o1234)))
   (expand-format
    (assert-equal "xx551,234"
 		 (format nil "~9,'x:o" #o551234)))
   (expand-format
    (assert-equal "xx66,551,234"
 		 (format nil "~12,'x:o" #o66551234)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx66a551a234"
 		 (format nil "~12,'x,'a:o" #o66551234)))

  )

(define-test binary
  ;; test that the mincol parameter is taken into account
  (expand-format
   (assert-equal "101"
		 (format nil "~1b" #b101)))
  (expand-format
   (assert-equal "101"
		 (format nil "~2b" #b101)))
  (expand-format
   (assert-equal "101"
		 (format nil "~3b" #b101)))
  (expand-format
   (assert-equal " 101"
		 (format nil "~4b" #b101)))
  (expand-format
   (assert-equal "  101"
		 (format nil "~5b" #b101)))
  ;; test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "xx101"
		 (format nil "~5,'xb" #b101)))
  ;; test the : modifier
  (expand-format
   (assert-equal "xx101"
		 (format nil "~5,'x:b" #b101)))
   (expand-format
    (assert-equal "xx1,011"
 		 (format nil "~7,'x:b" #b1011)))
   (expand-format
    (assert-equal "xx111,011"
 		 (format nil "~9,'x:b" #b111011)))
   (expand-format
    (assert-equal "xx10,111,011"
 		 (format nil "~12,'x:b" #b10111011)))
   ;; test the commachar parameter is taken into account
   (expand-format
    (assert-equal "xx10a111a011"
 		 (format nil "~12,'x,'a:b" #b10111011)))

  )

(define-test aesthetic
  ;; Test that objects are printed as with princ
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
	do (expand-format
	    (assert-equal (princ-to-string obj)
			  (format nil "~a" obj))))
  ;; Test that with a `:' modifier, NIL will print as ()
  (expand-format
   (assert-equal "()"
		 (format nil "~:a" nil)))
  ;; Test that the mincol argument is taken into account
  (expand-format
   (assert-equal "hello  "
		 (format nil "~7a" "hello")))
  ;; Test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "  hello"
		 (format nil "~7@a" "hello")))
  ;; Test that the colinc parameter is taken into account
  (expand-format
   (assert-equal "hello                     "
		 (format nil "~21,7a" "hello")))
  ;; Test that the minpad parameter is taken into account
  (expand-format
   (assert-equal "hello   "
		 (format nil "~,,3a" "hello")))
  ;; Test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "helloxxx"
		 (format nil "~0,1,3,'xa" "hello")))
  )

(define-test standard
  ;; Test that objects are printed as with princ
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
	do (expand-format
	    (assert-equal (prin1-to-string obj)
			  (format nil "~s" obj))))
  ;; Test that with a `:' modifier, NIL will print as ()
  (expand-format
   (assert-equal "()"
		 (format nil "~:s" nil)))
  ;; Test that the mincol argument is taken into account
  (expand-format
   (assert-equal "12345  "
		 (format nil "~7s" 12345)))
  ;; Test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "  12345"
		 (format nil "~7@s" 12345)))
  ;; Test that the colinc parameter is taken into account
  (expand-format
   (assert-equal "12345                     "
		 (format nil "~21,7s" 12345)))
  ;; Test that the minpad parameter is taken into account
  (expand-format
   (assert-equal "12345   "
		 (format nil "~,,3s" 12345)))
  ;; Test that the padchar parameter is taken into account
  (expand-format
   (assert-equal "12345xxx"
		 (format nil "~0,1,3,'xs" 12345)))
  )

(define-test write
  ;; Test that objects are renendered as with write.
  (loop for obj in '(234 -10 1.5 'abc "hello" #\x #\Space nil)
	do (expand-format
	    (assert-equal (with-output-to-string (stream)
			    (write obj :stream stream))
			  (format nil "~s" obj))))
  ;; test that it can handle circular lists
  (let ((*print-circle* t)) 
    (expand-format
     (assert-equal "#1=(1 . #1#)"
		   (format nil "~w" 
			   (let ((l (list 1)))
			     (setf (cdr l) l))))))
  
  ;; test that this directive reports an error
  ;; if a parameter is given
  (assert-error 'error (format nil "~1w" 234))
  (assert-error 'error (format nil "~'aw" 234))
  )

(define-test go-to
  (expand-format
   (assert-equal "ac"
		 (format nil "~c~*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ab"
		 (format nil "~c~0*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ac"
		 (format nil "~c~1*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ad"
		 (format nil "~c~2*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aa"
		 (format nil "~c~:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "ab"
		 (format nil "~c~0:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aa"
		 (format nil "~c~1:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aba"
		 (format nil "~c~c~2:*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "aba"
		 (format nil "~c~c~@*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "abb"
		 (format nil "~c~c~1@*~c" #\a #\b #\c #\d)))
  (expand-format
   (assert-equal "abd"
		 (format nil "~c~c~3@*~c" #\a #\b #\c #\d)))
  ;; Test that going beyond the first or last argument
  ;; gives an error.
  (assert-error 'error
		(format nil "~c~c~*" #\a #\b))
  (assert-error 'error
		(format nil "~c~c~2*~:2*~c" #\a #\b #\c))
  (assert-error 'error
		(format nil "~c~:2*~2*~c" #\a #\b #\c))
  (assert-error 'error
		(format nil "~c~-1@*~0@*~c" #\a #\b #\c))
  (assert-error 'error
		(format nil "~c~4@*~0@*~c" #\a #\b #\c))
  )

(define-test conditional
  (expand-format
   (assert-equal "abc"
		 (format nil "~[xyz~;abc~;def~]" 1)))
  (expand-format
   (assert-equal "xyz"
		 (format nil "~[xyz~;abc~;def~]" 0)))
  (expand-format
   (assert-equal ""
		 (format nil "~[xyz~;abc~;def~]" 3)))
  ;; test the default clause
  (expand-format
   (assert-equal "abc"
		 (format nil "~[xyz~;abc~:;def~]" 1)))
  (expand-format
   (assert-equal "xyz"
		 (format nil "~[xyz~;abc~:;def~]" 0)))
  (expand-format
   (assert-equal "def"
		 (format nil "~[xyz~;abc~:;def~]" 3)))
  (expand-format
   (assert-equal "abc"
		 (format nil "~:[xyz~;abc~]" nil)))
  (expand-format
   (assert-equal "xyz"
		 (format nil "~:[xyz~;abc~]" 24)))
  (expand-format
   (assert-equal "xyz23"
		 (format nil "~@[xyz~]~d" 23)))
  (expand-format
   (assert-equal "23"
		 (format nil "~@[xyz~]~d" nil 23)))
  ;; test the use of the parameter instead of the argument
  (expand-format
   (assert-equal "abc"
		 (format nil "~#[xyz~;abc~;def~]" 10)))
  (expand-format
   (assert-equal "xyz"
		 (format nil "~#[xyz~;abc~;def~]")))
  (expand-format
   (assert-equal ""
		 (format nil "~#[xyz~;abc~;def~]" 10 10 10)))
  (expand-format
   (assert-equal "abc"
		 (format nil "~v[xyz~;abc~;def~]" 1)))
  (expand-format
   (assert-equal "xyz"
		 (format nil "~v[xyz~;abc~;def~]" 0)))
  (expand-format
   (assert-equal ""
		 (format nil "~v[xyz~;abc~;def~]" 3)))
  (expand-format
   (assert-equal "abc"
		 (format nil "~1[xyz~;abc~;def~]" 10)))
  (expand-format
   (assert-equal "xyz"
		 (format nil "~0[xyz~;abc~;def~]" 10)))
  (expand-format
   (assert-equal ""
		 (format nil "~3[xyz~;abc~;def~]" 10)))
  ;; test that giving the : modifier fails if there
  ;; are not exactly two clauses
  (assert-error 'error
		(format nil "~:[xyz~;abc~;def~]" nil))
  (assert-error 'error
		(format nil "~:[xyz~]" nil))
  ;; test that giving the @ modifier fails if there
  ;; is not exactly one clause
  (assert-error 'error
		(format nil "~@[xyz~;abc~]~d" nil 23))
  ;; test that giving no clauses fails
  (assert-error 'error
		(format nil "~[~]" nil 23))
  ;; test that giving both modifiers gives an error.
  (assert-error 'error
		(format nil "~:@[xyz~;abc~;def~]" 1 2 3))
  ;; test that giving the : modifier to a clause separator
  ;; other than the last gives an error
  (assert-error 'error
		(format nil "~[xyz~:;abc~:;def~]" 3))
  ;; test that giving the modifiers to ~] gives an error
  ;; test that giving parameters to ~; or ~] gives an error
  (assert-error 'error
		(format nil "~[xyz~;abc~;def~2]" 3))
  (assert-error 'error
		(format nil "~[xyz~;abc~2;def~]" 3))
  (assert-error 'error
		(format nil "~[xyz~;abc~;def~#]" 3))
  (assert-error 'error
		(format nil "~[xyz~;abc~#;def~]" 3))
  (assert-error 'error
		(format nil "~[xyz~;abc~;def~v]" 3))
  (assert-error 'error
		(format nil "~[xyz~;abc~v;def~]" 3))
  )

(define-test iteration
  (expand-format 
   (assert-equal "ABCDE"
		 (format nil "~{~a~a~}~a" '(a b c d) 'e)))
  ;; test that, with a parameter, at most that many
  ;; iterations are done.
  (expand-format 
   (assert-equal "ABE"
		 (format nil "~1{~a~a~}~a" '(a b c d) 'e)))
  (expand-format 
   (assert-equal "E"
		 (format nil "~0{~a~a~}~a" '(a b c d) 'e)))
  ;; test that the `:' modifier is taken into account
  (expand-format
   (assert-equal "ABCDE"
		 (format nil "~:{~a~a~}~a" '((a b 1) (c d 2)) 'e)))
  (expand-format
   (assert-equal "ABE"
		 (format nil "~1:{~a~a~}~a" '((a b 1) (c d 2)) 'e)))
  (expand-format
   (assert-equal "E"
		 (format nil "~0:{~a~a~}~a" '((a b 1) (c d 2)) 'e)))
  ;; test that the `@' modifier is taken into account
  (expand-format
   (assert-equal "ABCD"
		 (format nil "~@{~a~a~}" 'a 'b 'c 'd)))
  (expand-format
   (assert-equal "ABC"
		 (format nil "~1@{~a~a~}~a" 'a 'b 'c 'd 'e)))
  (expand-format
   (assert-equal "A"
		 (format nil "~0@{~a~a~}~a" 'a 'b 'c 'd 'e)))
  ;; test that using both modifiers is taken into account
  (expand-format
   (assert-equal "ABCD"
		 (format nil "~:@{~a~a~}" '(a b) '(c d))))
  (expand-format
   (assert-equal "ABE"
		 (format nil "~1:@{~a~a~}~a" '(a b) 'e)))
  (expand-format
   (assert-equal "E"
		 (format nil "~0:@{~a~a~}~a" 'e)))
  )
