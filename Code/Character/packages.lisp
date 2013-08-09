(cl:in-package #:common-lisp-user)

(defpackage #:sicl-character
  (:use #:common-lisp)
  (:shadow
   . #1=(#:character
	 #:characterp
	 #:base-char
	 #:standard-char
	 #:extended-char
	 #:alpha-char-p
	 #:alphanumericp
	 #:digit-char-p
	 #:graphic-char-p
	 #:standard-char-p
	 #:char-upcase
	 #:char-downcase
	 #:upper-case-p
	 #:lower-case-p
	 #:both-case-p
	 #:char=
	 #:char/=
	 #:char<
	 #:char>
	 #:char<=
	 #:char>=
	 #:char-equal
	 #:char-not-equal
	 #:char-lessp
	 #:char-greaterp
	 #:char-not-greaterp
	 #:char-not-lessp
	 #:char-code
	 #:char-int
	 #:code-char
	 #:char-code-limit
	 #:char-name
	 #:name-char))
  (:export . #1#))
