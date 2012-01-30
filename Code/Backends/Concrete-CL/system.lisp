(in-package #:sicl-system)

;;; The value cells of these symbols will contain the corresponding
;;; class objects.

(defunbound *class-fixnum*)

(defunbound *class-cons*)

(defunbound *class-character*)

(defunbound *class-vector*)

(defunbound *class-string*)

(defunbound *class-symbol*)

(defunbound *class-package*)

(defunbound *class-code*)

(defunbound *class-function*)

(defunbound *class-dynamic-frame*)

(defunbound *class-unwind-protect*)

;;; The value cells of these symbols will contain vectors containing
;;; machine instructions for various linkage purposes.

(defunbound *linkage-error*)

(defunbound *linkage-function*)

(defunbound *linkage-symbol*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Word.
;;;
;;; A word is the machine representation of any data item.
;;; It can be interpreted differently in different situation. 
;;; As far as the host system is concerned, a word is a positive 
;;; number between 0 and 2^n-1 where n is the number of bits in
;;; the word.  To represent negative numbers, we use 2-complement.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +word-size+ 64)
  (defconstant +word-size-in-bytes+ (/ +word-size+ 8)))

(deftype word () `(unsigned-byte ,+word-size+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Machine operations on words

(defun word-from-unsigned-host-number (host-number)
  (assert (<= 0
	      host-number
	      (1- (ash 1 +word-size+))))
  host-number)

(defun word-from-signed-host-number (host-number)
  (assert (<= (- (ash 1 (1- +word-size+)))
	      host-number
	      (1- (ash 1 (1- +word-size+)))))
  (if (minusp host-number)
      (+ host-number (ash 1 +word-size+))
      host-number))

(defun signed-host-number-from-word (word)
  (assert (typep word 'word))
  (if (>= word (ash 1 (1- +word-size+)))
      (- word (ash 1 +word-size+))
      word))

(defun u+ (x y &optional (carry 0))
  (declare (type word x y)
	   (type bit carry))
  (let ((sum (+ x y carry)))
    (if (>= sum (ash 1 +word-size+))
	(values (- sum (ash 1 +word-size+)) 1)
	(values sum 0))))

(defun u- (x y &optional (carry 0))
  (declare (type word x y)
	   (type bit carry))
  (let ((diff (- x y carry)))
    (if (minusp diff)
	(values (+ diff (ash 1 +word-size+)) 1)
	(values diff 0))))

(defun s+ (x y &optional (carry 0))
  (declare (type word x y)
	   (type bit carry))
  (let ((xh (signed-host-number-from-word x))
	(yh (signed-host-number-from-word y)))
    (let ((sum (+ xh yh carry)))
      (cond ((>= sum (ash 1 (1- +word-size+)))
	     (values (- sum (ash 1 (1- +word-size+))) 1))
	    ((< sum (- (ash 1 (1- +word-size+))))
	     (values (+ sum (ash 1 (1- +word-size+))) 1))
	    (t
	     (values sum 0))))))

(defun s- (x y &optional (carry 0))
  (declare (type word x y)
	   (type bit carry))
  (let ((xh (signed-host-number-from-word x))
	(yh (signed-host-number-from-word y)))
    (let ((diff (- xh yh carry)))
      (cond ((>= diff (ash 1 (1- +word-size+)))
	     (values (- diff (ash 1 (1- +word-size+))) 1))
	    ((< diff (- (ash 1 (1- +word-size+))))
	     (values (+ diff (ash 1 (1- +word-size+))) 1))
	    (t
	     (values diff 0))))))

(defun neg (x)
  (declare (type word x))
  (s- 0 x))

(defun u* (x y)
  (declare (type word x y))
  (let ((product (* x y)))
    (values (logand product (1- (ash 1 +word-size+)))
	    (ash product (- +word-size+)))))

(defun s* (x y)
  (declare (type word x y))
  (let ((xh (signed-host-number-from-word x))
	(yh (signed-host-number-from-word y)))
    (let ((product (* xh yh)))
      (values (logand product (1- (ash 1 +word-size+)))
	      (logand (ash product (- +word-size+)) (1- (ash 1 +word-size+)))))))

(defun logshift (x y)
  (declare (type word x y))
  (let ((yh (signed-host-number-from-word y)))
    (logand (ash x yh) (1- (ash 1 +word-size+)))))

(defun arshift (x y)
  (declare (type word x y))
  (let ((xh (signed-host-number-from-word x))
	(yh (signed-host-number-from-word y)))
    (logand (ash xh yh) (1- (ash 1 +word-size+)))))

(defun band (x y)
  (declare (type word x y))
  (logand x y))

(defun bior (x y)
  (declare (type word x y))
  (logior x y))

(defun bxor (x y)
  (declare (type word x y))
  (logxor x y))

(defun bnot (x)
  (declare (type word x))
  (lognot x))
