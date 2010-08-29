(in-package #:sicl-read-test)

(define-test read.symbol.followed.by.end.of.file
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "abc")
				 (read stream)))
		  "ABC")))

(define-test read.symbol.followed.by.whitespace
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "abc ")
				 (read stream)))
		  "ABC")))

(define-test read.symbol.followed.by.macro-character
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "abc(")
				 (read stream)))
		  "ABC")))

(define-test read.symbol.with.single.escape.at.the.beginning
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "\\abc(")
				 (read stream)))
		  "aBC")))

(define-test read.symbol.with.single.escape.in.the.middle
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "a\\bc(")
				 (read stream)))
		  "AbC")))

(define-test read.symbol.with.single.escape.at.the.end
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "ab\\c(")
				 (read stream)))
		  "ABc")))

(define-test read.symbol.with.single.escape.middle.macro.char
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "a\\(c(")
				 (read stream)))
		  "A(C")))

(define-test read.symbol.with.single.escape.end.of.file
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc\\")
				 (read stream))))))

(define-test read.symbol.with.double-escape.at.the.beginning
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "|a|bc")
				 (read stream)))
		  "aBC")))

(define-test read.symbol.with.double-escape.in.the.middle
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "a|b|c")
				 (read stream)))
		  "AbC")))

(define-test read.symbol.with.double-escape.at.the.end
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-equal (symbol-name (with-input-from-string (stream "ab|c|")
				 (read stream)))
		  "ABc")))

(define-test read.symbol.with.double.escape.end.of.file.1
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "ab|c")
				 (read stream))))))

(define-test read.symbol.with.double.escape.end.of.file.2
  (let ((*read-base* 10)
	(*read-table* (copy-readtable nil)))
    (assert-error
     'reader-error
     (symbol-name (with-input-from-string (stream "abc|")
				 (read stream))))))

