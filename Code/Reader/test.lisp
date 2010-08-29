(in-package #:sicl-read-test)

(define-test read.symbol.followed.by.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABC"
		  (symbol-name (with-input-from-string (stream "abc")
				 (read stream))))))

(define-test read.symbol.followed.by.whitespace
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABC"
		  (symbol-name (with-input-from-string (stream "abc ")
				 (read stream))))))

(define-test read.symbol.followed.by.macro-character
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABC"
		  (symbol-name (with-input-from-string (stream "abc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.beginning
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "aBC"
		  (symbol-name (with-input-from-string (stream "\\abc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.in.the.middle
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "a\\bc(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.at.the.end
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABc"
		  (symbol-name (with-input-from-string (stream "ab\\c(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.middle.macro.char
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "A(C"
		  (symbol-name (with-input-from-string (stream "a\\(c(")
				 (read stream))))))

(define-test read.symbol.with.single.escape.end.of.file
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc\\")
				 (read stream))))))

(define-test read.symbol.with.double-escape.at.the.beginning
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "aBC"
		  (symbol-name (with-input-from-string (stream "|a|bc")
				 (read stream))))))

(define-test read.symbol.with.double-escape.in.the.middle
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "AbC"
		  (symbol-name (with-input-from-string (stream "a|b|c")
				 (read stream))))))

(define-test read.symbol.with.double-escape.at.the.end
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-equal "ABc"
		  (symbol-name (with-input-from-string (stream "ab|c|")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.1
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "ab|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.2
  (let ((*read-base* 10)
	(*readtable* (copy-readtable nil)))
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
