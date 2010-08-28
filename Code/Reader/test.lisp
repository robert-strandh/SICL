(in-package #:sicl-read-test)

(define-test read.symbol.followed.by.end.of.file
  (assert-equal (symbol-name (with-input-from-string (stream "abc")
			       (read stream)))
		"ABC"))

(define-test read.symbol.followed.by.whitespace
  (assert-equal (symbol-name (with-input-from-string (stream "abc ")
			       (read stream)))
		"ABC"))

(define-test read.symbol.followed.by.macro-character
  (assert-equal (symbol-name (with-input-from-string (stream "abc(")
			       (read stream)))
		"ABC"))

(define-test read.symbol.with.single.escape.at.the.beginning
  (assert-equal (symbol-name (with-input-from-string (stream "\\abc(")
			       (read stream)))
		"aBC"))


(define-test read.symbol.with.single.escape.in.the.middle
  (assert-equal (symbol-name (with-input-from-string (stream "a\\bc(")
			       (read stream)))
		"AbC"))

(define-test read.symbol.with.single.escape.at.the.end
  (assert-equal (symbol-name (with-input-from-string (stream "ab\\c(")
			       (read stream)))
		"ABc"))

(define-test read.symbol.with.single.escape.middle.macro.char
  (assert-equal (symbol-name (with-input-from-string (stream "a\\(c(")
			       (read stream)))
		"A(C"))

