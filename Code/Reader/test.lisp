(in-package #:sicl-read-test)

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

(define-test read.symbol.followed.by.macro-character.upcase
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

(define-test read.symbol.with.double-escape.at.the.beginning.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "aBC"
		  (symbol-name (with-input-from-string (stream "|a|bc")
				 (read stream))))))

(define-test read.symbol.with.double-escape.in.the.middle.upcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "a|b|c")
				 (read stream))))))

(define-test read.symbol.with.double-escape.at.the.end.upcase
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

(define-test read.symbol.followed.by.macro-character.downcase
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

(define-test read.symbol.with.double-escape.at.the.beginning.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "|A|BC")
				 (read stream))))))

(define-test read.symbol.with.double-escape.in.the.middle.downcase
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :downcase)
    (assert-equal "aBc"
		  (symbol-name (with-input-from-string (stream "A|B|C")
				 (read stream))))))

(define-test read.symbol.with.double-escape.at.the.end.downcase
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

(define-test read.symbol.followed.by.macro-character.preserve
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

(define-test read.symbol.with.double-escape.at.the.beginning.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "aBC"
		  (symbol-name (with-input-from-string (stream "|a|bc")
				 (read stream))))))

(define-test read.symbol.with.double-escape.in.the.middle.preserve
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (assert-equal "Abc"
		  (symbol-name (with-input-from-string (stream "A|b|c")
				 (read stream))))))

(define-test read.symbol.with.double-escape.at.the.end.preserve
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
