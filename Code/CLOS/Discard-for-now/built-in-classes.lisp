(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Built-in classes

(make-built-in-class 'number '(t) '(t))
(make-built-in-class 'real '(number) '(number t))
(make-built-in-class 'rational '(real) '(real number t))
(make-built-in-class 'integer '(rational) '(rational real number t))
(make-built-in-class 'ratio '(rational) '(rational real number t))
(make-built-in-class 'float '(real) '(real number t))
(make-built-in-class 'complex '(number) '(number t))
(make-built-in-class 'character '(t) '(t))
(make-built-in-class 'symbol '(t) '(t))
(make-built-in-class 'sequence '(t) '(t))
(make-built-in-class 'list '(sequence) '(sequence t))
(make-built-in-class 'null '(symbol list) '(symbol list sequence t))
(make-built-in-class 'cons '(list sequence) '(list sequence t))
(make-built-in-class 'array '(t) '(t))
(make-built-in-class 'vector '(array sequence) '(array sequence t))
(make-built-in-class 'bit-vector '(vector) '(vector array sequence t))
(make-built-in-class 'string '(vector) '(vector array sequence t))
(make-built-in-class 'package '(t) '(t))
(make-built-in-class 'hash-table '(t) '(t))
(make-built-in-class 'stream '(t) '(t))
(make-built-in-class 'broadcast-stream '(t) '(stream t))
(make-built-in-class 'concatenated-stream '(t) '(stream t))
(make-built-in-class 'echo-stream '(t) '(stream t))
(make-built-in-class 'file-stream '(t) '(stream t))
(make-built-in-class 'synonym-stream '(t) '(stream t))
(make-built-in-class 'two-way-stream '(t) '(stream t))

;;; FIXME: there are some more, but this will do for now.

(defun built-in-class-of (object)
  (find-class 
   (cond ((integerp object) 'integer)
	 ((rationalp object) 'ratio)
	 ((complexp object) 'complex)
	 ((floatp object) 'float)
	 ((characterp object) 'character)
	 ((null object) 'null)
	 ((symbolp object) 'symbol)
	 ((consp object) 'cons)
	 ((stringp object) 'string)
	 ((bit-vector-p object) 'bit-vector)
	 ((vectorp object) 'vector)
	 ((packagep object) 'package)
	 ((hash-table-p object) 'hash-table)
	 ((streamp object) 'stream) ; can't do better
	 (t nil))))

