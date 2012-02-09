(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Built-in classes

(defclass number (t)
  ()
  (:metaclass built-in-class))

(defclass real (number)
  ()
  (:metaclass built-in-class))


(defclass rational (real)
  ()
  (:metaclass built-in-class))


(defclass integer (rational)
  ()
  (:metaclass built-in-class))
  
(defclass ratio (rational)
  ()
  (:metaclass built-in-class))
  
(defclass float (real)
  ()
  (:metaclass built-in-class))
  
(defclass complex (number)
  ()
  (:metaclass built-in-class))

(defclass character (t)
  ()
  (:metaclass built-in-class))

(defclass symbol (t)
  ()
  (:metaclass built-in-class))

(defclass sequence (t)
  ()
  (:metaclass built-in-class))

(defclass list (sequence)
  ()
  (:metaclass built-in-class))

(defclass null (symbol list)
  ()
  (:metaclass built-in-class))

(defclass cons (list)
  ()
  (:metaclass built-in-class))

(defclass array (t)
  ()
  (:metaclass built-in-class))

(defclass vector (array sequence)
  ()
  (:metaclass built-in-class))

(defclass bit-vector (vector)
  ()
  (:metaclass built-in-class))

(defclass string (vector)
  ()
  (:metaclass built-in-class))

(defclass package (t)
  ()
  (:metaclass built-in-class))

(defclass hash-table (t)
  ()
  (:metaclass built-in-class))

(defclass stream (t)
  ()
  (:metaclass built-in-class))

(defclass broadcast-stream (stream)
  ()
  (:metaclass built-in-class))

(defclass concatenated-stream (stream)
  ()
  (:metaclass built-in-class))

(defclass echo-stream (stream)
  ()
  (:metaclass built-in-class))

(defclass file-stream (stream)
  ()
  (:metaclass built-in-class))

(defclass string-stream (stream)
  ()
  (:metaclass built-in-class))

(defclass synonym-stream (stream)
  ()
  (:metaclass built-in-class))

(defclass two-way-stream (stream)
  ()
  (:metaclass built-in-class))

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

