(cl:in-package #:sicl-read-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading symbols

(define-test read.symbol.followed.by.end.of.file.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABC"
		  (symbol-name (with-input-from-string (stream "abc")
				 (read stream))))))

(define-test read.symbol.followed.by.whitespace.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABC"
		  (symbol-name (with-input-from-string (stream "abc ")
				 (read stream))))))

(define-test read.symbol.followed.by.macro.character.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABC"
		  (symbol-name (with-input-from-string (stream "abc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.beginning.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "aBC"
		  (symbol-name (with-input-from-string (stream "\\abc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.in.the.middle.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "a\\bc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.end.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABc"
		  (symbol-name (with-input-from-string (stream "ab\\c(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.middle.macro.char.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "A(C"
		  (symbol-name (with-input-from-string (stream "a\\(c(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.end.of.file.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc\\")
				 (read stream))))))

(define-test read.symbol.with.double.escape.at.the.beginning.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "aBC"
		  (symbol-name (with-input-from-string (stream "|a|bc")
				 (read stream))))))

(define-test read.symbol.with.double.escape.in.the.middle.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "a|b|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.at.the.end.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABc"
		  (symbol-name (with-input-from-string (stream "ab|c|")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.upcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "ab|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.upcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc|")
				 (read stream))))))

(define-test read.symbol.followed.by.end.of.file.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "abc"
		  (symbol-name (with-input-from-string (stream "ABC")
				 (read stream))))))

(define-test read.symbol.followed.by.whitespace.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "abc"
		  (symbol-name (with-input-from-string (stream "ABC ")
				 (read stream))))))

(define-test read.symbol.followed.by.macro.character.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "abc"
		  (symbol-name (with-input-from-string (stream "ABC(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.beginning.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "\\ABC(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.in.the.middle.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "aBc"
		  (symbol-name (with-input-from-string (stream "A\\BC(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.end.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "abC"
		  (symbol-name (with-input-from-string (stream "AB\\C(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.middle.macro.char.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "a(c"
		  (symbol-name (with-input-from-string (stream "A\\(C(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.end.of.file.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc\\")
				 (read stream))))))

(define-test read.symbol.with.double.escape.at.the.beginning.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "|A|BC")
				 (read stream))))))

(define-test read.symbol.with.double.escape.in.the.middle.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "aBc"
		  (symbol-name (with-input-from-string (stream "A|B|C")
				 (read stream))))))

(define-test read.symbol.with.double.escape.at.the.end.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "abC"
		  (symbol-name (with-input-from-string (stream "AB|C|")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.downcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "ab|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.downcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc|")
				 (read stream))))))

(define-test read.symbol.followed.by.end.of.file.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "AbC")
				 (read stream))))))

(define-test read.symbol.followed.by.whitespace.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "AbC ")
				 (read stream))))))

(define-test read.symbol.followed.by.macro.character.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "AbC(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.beginning.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "\\AbC(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.in.the.middle.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "A\\bc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.end.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "Ab\\c(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.middle.macro.char.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "A(c"
		  (symbol-name (with-input-from-string (stream "A\\(c(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.end.of.file.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc\\")
				 (read stream))))))

(define-test read.symbol.with.double.escape.at.the.beginning.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "aBc"
		  (symbol-name (with-input-from-string (stream "|a|Bc")
				 (read stream))))))

(define-test read.symbol.with.double.escape.in.the.middle.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "A|b|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.at.the.end.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "Ab|c|")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.preserve.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "ab|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.preserve.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc|")
				 (read stream))))))

(define-test read.symbol.starting.as.integer.end.of.file.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "123AB"
		  (symbol-name (with-input-from-string (stream "123Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.integer.followed.by.whitespace.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "123AB"
		  (symbol-name (with-input-from-string (stream "123Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.end.of.file.upcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12/AB"
		  (symbol-name (with-input-from-string (stream "12/Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.end.of.file.upcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12/3AB"
		  (symbol-name (with-input-from-string (stream "12/3Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.followed.by.whitespace.upcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12/AB"
		  (symbol-name (with-input-from-string (stream "12/Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.followed.by.whitespace.upcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12/3AB"
		  (symbol-name (with-input-from-string (stream "12/3Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.float.end.of.file.upcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12.AB"
		  (symbol-name (with-input-from-string (stream "12.Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.float.end.of.file.upcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12.3AB"
		  (symbol-name (with-input-from-string (stream "12.3Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.float.followed.by.whitespace.upcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12.AB"
		  (symbol-name (with-input-from-string (stream "12.Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.float.followed.by.whitespace.upcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "12.3AB"
		  (symbol-name (with-input-from-string (stream "12.3Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.integer.end.of.file.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "123ab"
		  (symbol-name (with-input-from-string (stream "123Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.integer.followed.by.whitespace.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "123ab"
		  (symbol-name (with-input-from-string (stream "123Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.end.of.file.downcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12/ab"
		  (symbol-name (with-input-from-string (stream "12/Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.end.of.file.downcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12/3ab"
		  (symbol-name (with-input-from-string (stream "12/3Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.followed.by.whitespace.downcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12/ab"
		  (symbol-name (with-input-from-string (stream "12/Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.followed.by.whitespace.downcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12/3ab"
		  (symbol-name (with-input-from-string (stream "12/3Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.float.end.of.file.downcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12.ab"
		  (symbol-name (with-input-from-string (stream "12.Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.float.end.of.file.downcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12.3ab"
		  (symbol-name (with-input-from-string (stream "12.3Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.float.followed.by.whitespace.downcase.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12.ab"
		  (symbol-name (with-input-from-string (stream "12.Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.float.followed.by.whitespace.downcase.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "12.3ab"
		  (symbol-name (with-input-from-string (stream "12.3Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.integer.end.of.file.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "123Ab"
		  (symbol-name (with-input-from-string (stream "123Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.integer.followed.by.whitespace.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "123Ab"
		  (symbol-name (with-input-from-string (stream "123Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.end.of.file.preserve.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12/Ab"
		  (symbol-name (with-input-from-string (stream "12/Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.end.of.file.preserve.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12/3Ab"
		  (symbol-name (with-input-from-string (stream "12/3Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.followed.by.whitespace.preserve.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12/Ab"
		  (symbol-name (with-input-from-string (stream "12/Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.ratio.followed.by.whitespace.preserve.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12/3Ab"
		  (symbol-name (with-input-from-string (stream "12/3Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.float.end.of.file.preserve.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12.Ab"
		  (symbol-name (with-input-from-string (stream "12.Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.float.end.of.file.preserve.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12.3Ab"
		  (symbol-name (with-input-from-string (stream "12.3Ab")
				 (read stream))))))

(define-test read.symbol.starting.as.float.followed.by.whitespace.preserve.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12.Ab"
		  (symbol-name (with-input-from-string (stream "12.Ab ")
				 (read stream))))))

(define-test read.symbol.starting.as.float.followed.by.whitespace.preserve.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "12.3Ab"
		  (symbol-name (with-input-from-string (stream "12.3Ab ")
				 (read stream))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading integers

(define-test read.integer.zero.no.sign.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 0
		  (with-input-from-string (stream "0")
				 (read stream)))))

(define-test read.integer.zero.no.sign.followed.by.space
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 0
		  (with-input-from-string (stream "0 ")
				 (read stream)))))

(define-test read.integer.zero.no.sign.followed.by.macro.char
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 0
		  (with-input-from-string (stream "0(")
				 (read stream)))))

(define-test read.integer.zero.with.minus.sign.followed.by.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 0
		  (with-input-from-string (stream "-0")
				 (read stream)))))

(define-test read.positive.integer.no.sign.followed.by.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 123
		  (with-input-from-string (stream "123")
				 (read stream)))))

(define-test read.positive.integer.with.sign.followed.by.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 123
		  (with-input-from-string (stream "+123")
				 (read stream)))))

(define-test read.negative.integer.followed.by.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal -123
		  (with-input-from-string (stream "-123")
				 (read stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading ratios

(define-test read.ratio.zero.no.sign.followed.by.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 0
		  (with-input-from-string (stream "0/1")
				 (read stream)))))

(define-test read.ratio.zero.no.sign.followed.by.space
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 0
		  (with-input-from-string (stream "0/1 ")
				 (read stream)))))

(define-test read.positive.ratio.no.sign.followed.by.end-of-file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 2/3
		  (with-input-from-string (stream "2/3")
				 (read stream)))))
  
(define-test read.positive.ratio.plus.sign.followed.by.end-of-file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal 2/3
		  (with-input-from-string (stream "+2/3")
				 (read stream)))))
  
(define-test read.negative.ratio.followed.by.end-of-file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal -2/3
		  (with-input-from-string (stream "-2/3")
				 (read stream)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading floats

(define-test read.positive.float.no.sign.no.exponent.marker.followed.by.end.of.file.1
  (assert-true (let ((*read-base* 10)
		     (*readtable* (copy-readtable nil))
		     (*read-default-float-format* 'short-float))
		 (let ((result (with-input-from-string (stream ".2")
				 (read stream))))
		   (and (typep result 'short-float)
			(= result
			   (/ (float 2 1s0) (float 10 1s0))))))))

(define-test read.positive.float.no.sign.no.exponent.marker.followed.by.end.of.file.2
  (assert-true (let ((*read-base* 10)
		     (*readtable* (copy-readtable nil))
		     (*read-default-float-format* 'short-float))
		 (let ((result (with-input-from-string (stream "0.2")
				 (read stream))))
		   (and (typep result 'short-float)
			(= result
			   (/ (float 2 1s0) (float 10 1s0))))))))

(define-test read.positive.float.plus.sign.no.exponent.marker.followed.by.end.of.file.1
  (assert-true (let ((*read-base* 10)
		     (*readtable* (copy-readtable nil))
		     (*read-default-float-format* 'short-float))
		 (let ((result (with-input-from-string (stream "+.2")
				 (read stream))))
		   (and (typep result 'short-float)
			(= result
			   (/ (float 2 1s0) (float 10 1s0))))))))

(define-test read.positive.float.plus.sign.no.exponent.marker.followed.by.end.of.file.2
  (assert-true (let ((*read-base* 10)
		     (*readtable* (copy-readtable nil))
		     (*read-default-float-format* 'short-float))
		 (let ((result (with-input-from-string (stream "+0.2")
				 (read stream))))
		   (and (typep result 'short-float)
			(= result
			   (/ (float 2 1s0) (float 10 1s0))))))))

(define-test read.negative.float.no.exponent.marker.followed.by.end.of.file.1
  (assert-true (let ((*read-base* 10)
		     (*readtable* (copy-readtable nil))
		     (*read-default-float-format* 'short-float))
		 (let ((result (with-input-from-string (stream "-.2")
				 (read stream))))
		   (and (typep result 'short-float)
			(= result
			   (/ (float -2 1s0) (float 10 1s0))))))))

(define-test read.negative.float.no.exponent.marker.followed.by.end.of.file.2
  (assert-true (let ((*read-base* 10)
		     (*readtable* (copy-readtable nil))
		     (*read-default-float-format* 'short-float))
		 (let ((result (with-input-from-string (stream "-0.2")
				 (read stream))))
		   (and (typep result 'short-float)
			(= result
			   (/ (float -2 1s0) (float 10 1s0))))))))

			      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reading parentheses

(define-test read.parenthesis.list.empty
  (let ((*read-base* 10)
        (*readtable* (copy-readtable nil)))
    (assert-equal nil
                  (with-input-from-string (stream "()")
                    (read stream)))))

(define-test read.parenthesis.list.non.empty
  (let ((*read-base* 10)
        (*readtable* (copy-readtable nil)))
    (assert-equal (list nil)
                  (with-input-from-string (stream "(())")
                    (read stream)))))

(define-test read.parenthesis.list.nested
  (let ((*read-base* 10)
        (*readtable* (copy-readtable nil)))
    (assert-equal (list (list nil))
                  (with-input-from-string (stream "((()))")
                    (read stream)))))

(define-test read.parenthesis.list.non.empty.cdr
  (let ((*read-base* 10)
        (*readtable* (copy-readtable nil)))
    (assert-equal (list nil nil)
                  (with-input-from-string (stream "(()())")
                    (read stream)))))
