(cl:in-package #:sicl-compiler-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions and classes for type descriptors.  
;;;
;;; A type descriptor describes the possible types of a single
;;; variable.  
;;;
;;; Type descriptors are immutable. 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Descriptors for real numbers.
;;;
;;; All these descriptors contain lower and upper bounds.  Such a
;;; bound can be either * meaning unbounded, a number X meaning X is
;;; included in the interval, or a rational number within parentheses
;;; (X) meaning X is excluded from the interval.

;;; To take the AND of two lower bounds, just take the MAX of the two,
;;; except that things get a bit more complicated because of the
;;; notation (X).
(defun lower-bound-and (x y)
  (cond ((eq x '*)
	 y)
	((eq y '*)
	 x)
	((and (numberp x) (numberp y))
	 (max x y))
	((and (listp x) (listp y))
	 (list (max (car x) (car y))))
	(t
	 (when (listp x)
	   (rotatef x y))
	 ;; Now x is of the form X and y is of the form (Y)
	 (if (<= x (car y))
	     y
	     x))))

;;; To take the AND of two upper bounds, just take the MIN of the two,
;;; except that things get a bit more complicated because of the
;;; notation (X).
(defun upper-bound-and (x y)
  (cond ((eq x '*)
	 y)
	((eq y '*)
	 x)
	((and (numberp x) (numberp y))
	 (min x y))
	((and (listp x) (listp y))
	 (list (min (car x) (car y))))
	(t
	 (when (listp x)
	   (rotatef x y))
	 ;; Now x is of the form X and y is of the form (Y)
	 (if (>= x (car y))
	     y
	     x))))

(defun non-integer-lower-bound-or (x y)
  (cond ((or (eq x '*) (eq y '*))
	 '*)
	((and (numberp x) (numberp y))
	 (cond ((= x y)
		x)
	       ((and (>= x 0) (>= y 0))
		(coerce 0 (type-of x)))
	       (t '*)))
	((and (listp x) (listp y))
	 (cond ((= (car x) (car y))
		(car x))
	       ((and (>= (car x) 0) (>= (car y) 0))
		(list (coerce 0 (type-of x))))
	       (t '*)))
	(t
	 (when (listp x)
	   (rotatef x y))
	 ;; Now x is of the form X and y is of the form (Y)
	 (cond ((and (> x 0) (>= (car y) 0))
		(list (coerce 0 (type-of x))))
	       ((and (>= x 0) (>= (car y) 0))
		(coerce 0 (type-of x)))
	       (t '*)))))

(defun non-integer-upper-bound-or (x y)
  (cond ((or (eq x '*) (eq y '*))
	 '*)
	((and (numberp x) (numberp y))
	 (cond ((= x y)
		x)
	       ((and (<= x 0) (<= y 0))
		(coerce 0 (type-of x)))
	       (t '*)))
	((and (listp x) (listp y))
	 (cond ((= (car x) (car y))
		(car x))
	       ((and (<= (car x) 0) (<= (car y) 0))
		(list (coerce 0 (type-of x))))
	       (t '*)))
	(t
	 (when (listp x)
	   (rotatef x y))
	 ;; Now x is of the form X and y is of the form (Y)
	 (cond ((and (< x 0) (<= (car y) 0))
		(list (coerce 0 (type-of x))))
	       ((and (<= x 0) (<= (car y) 0))
		(coerce 0 (type-of x)))
	       (t '*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Descriptors for rational numbers.
;;;
;;; A rational descriptor is either NIL, meaning rationals are not
;;; allowed, or a list of three elements.  The first element is NIL if
;;; only integers are allowed and T if ratios are allowed too.  The
;;; second and third elements are the lower and upper bounds.

(defun integer-lower-bound-or (x y)
  (cond ((or (eq x '*) (eq y '*))
	 '*)
	((and (numberp x) (numberp y))
	 (cond ((= x y)
		x)
	       ((and (>= x 0) (>= y 0))
		0)
	       ((and (>= x most-negative-fixnum)
		     (>= y most-negative-fixnum))
		most-negative-fixnum)
	       (t '*)))
	((and (listp x) (listp y))
	 (cond ((= (car x) (car y))
		(car x))
	       ((and (>= (car x) 0) (>= (car y) 0))
		(list (coerce 0 (type-of x))))
	       ((and (>= (car x) most-negative-fixnum)
		     (>= (car y) most-negative-fixnum))
		(list most-negative-fixnum))
	       (t '*)))
	(t
	 (when (listp x)
	   (rotatef x y))
	 ;; Now x is of the form X and y is of the form (Y)
	 (cond ((and (> x 0) (>= (car y) 0))
		(list 0))
	       ((and (>= x 0) (>= (car y) 0))
		0)
	       ((and (>= x most-negative-fixnum)
		     (>= (car y) most-negative-fixnum))
		most-negative-fixnum)
	       (t '*)))))

(defun integer-upper-bound-or (x y)
  (cond ((or (eq x '*) (eq y '*))
	 '*)
	((and (numberp x) (numberp y))
	 (cond ((= x y)
		x)
	       ((and (<= x 0) (<= y 0))
		0)
	       ((and (<= x most-positive-fixnum)
		     (<= y most-positive-fixnum))
		most-positive-fixnum)
	       (t '*)))
	((and (listp x) (listp y))
	 (cond ((= (car x) (car y))
		(car x))
	       ((and (<= (car x) 0) (<= (car y) 0))
		(list (coerce 0 (type-of x))))
	       ((and (<= (car x) most-positive-fixnum)
		     (<= (car y) most-positive-fixnum))
		(list most-positive-fixnum))
	       (t '*)))
	(t
	 (when (listp x)
	   (rotatef x y))
	 ;; Now x is of the form X and y is of the form (Y)
	 (cond ((and (< x 0) (<= (car y) 0))
		(list 0))
	       ((and (<= x 0) (<= (car y) 0))
		0)
	       ((and (<= x most-positive-fixnum)
		     (<= (car y) most-positive-fixnum))
		most-positive-fixnum)
	       (t '*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Combine rational descriptors using OR.

(defun rational-or (descriptor1 descriptor2)
  (cond ((null descriptor1)
	 descriptor2)
	((null descriptor2)
	 descriptor1)
	((and (null (first descriptor1)) (null (first descriptor2)))
	 (list nil (integer-lower-bound-or
		    (second descriptor1) (second descriptor2))
	       (integer-upper-bound-or
		(third descriptor1) (third descriptor2))))
	(t
	 (list t (non-integer-lower-bound-or
		  (second descriptor1) (second descriptor2))
	       (non-integer-upper-bound-or
		(third descriptor1) (third descriptor2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Combine rational descriptors using AND.

(defun rational-and (descriptor1 descriptor2)
  (cond ((or (null descriptor1) (null descriptor2))
	 nil)
	(t
	 (list (and (first descriptor1) (first descriptor2))
	       (lower-bound-and (second descriptor1) (second descriptor2))
	       (upper-bound-and (third descriptor1) (third descriptor2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Take the DIFF between two rational descriptors.

(defun ratio-lower-bound-<= (x y)
  (cond ((eq x '*)
	 t)
	((eq y '*)
	 nil)
	((and (numberp x) (numberp y))
	 (<= x y))
	((and (consp x) (consp y))
	 (<= (car x) (car y)))
	((consp x)
	 (< (car x) y))
	(t
	 (<= x (car y)))))

(defun ratio-upper-bound->= (x y)
  (cond ((eq x '*)
	 t)
	((eq y '*)
	 nil)
	((and (numberp x) (numberp y))
	 (>= x y))
	((and (consp x) (consp y))
	 (>= (car x) (car y)))
	((consp x)
	 (> (car x) y))
	(t
	 (>= x (car y)))))

(defun integer-lower-bound-<= (x y)
  (cond ((eq x '*)
	 t)
	((eq y '*)
	 nil)
	((and (numberp x) (numberp y))
	 (<= x y))
	((and (consp x) (consp y))
	 (<= (car x) (car y)))
	((consp x)
	 (< (car x) y))
	(t
	 (<= x (1+ (car y))))))

(defun integer-upper-bound->= (x y)
  (cond ((eq x '*)
	 t)
	((eq y '*)
	 nil)
	((and (numberp x) (numberp y))
	 (>= x y))
	((and (consp x) (consp y))
	 (>= (car x) (car y)))
	((consp x)
	 (> (car x) y))
	(t
	 (>= x (1- (car y))))))

(defun rational-diff (descriptor1 descriptor2)
  (cond ((null descriptor1)
	 nil)
	((null descriptor2)
	 descriptor1)
	(t
	 (destructuring-bind (t1 l1 u1) descriptor1
	   (destructuring-bind (t2 l2 u2) descriptor2
	     (cond ((and (eq t1 nil) (eq t2 nil))
		    ;; We have two integer intervals.
		    (cond ((and (integer-lower-bound-<= l2 l1)
				(integer-upper-bound->= u2 u1))
			   ;; The second interval covers all of the
			   ;; first interval.
			   nil)
			  ((integer-lower-bound-<= l2 l1)
			   ;; The upper bound of the second interval
			   ;; is necessarily less than the upper bound
			   ;; of the third interval, so the upper
			   ;; bound of the second interval is either a
			   ;; number or a CONS.
			   `(nil
			     ,(if (consp u2) (car u2) (1+ u2))
			     ,u1))
			  ((integer-upper-bound->= u2 u1)
			   ;; The lower bound of the second interval
			   ;; is necessarily greater than the lower
			   ;; bound of the third interval, so the
			   ;; lower bound of the second interval is
			   ;; either a number or a CONS.
			   `(nil
			     ,l1
			     ,(if (consp l2) (car l2) (1- l2))))
			  (t
			   ;; We are trying to remove an interval in
			   ;; the middle of a wider interval.  We give
			   ;; up and just allow the entire original
			   ;; interval.
			   descriptor1)))
		   ((and (eq t1 t) (eq t2 t))
		    ;; We have two ratio intervals.
		    (cond ((and (ratio-lower-bound-<= l2 l1)
				(ratio-upper-bound->= u2 u1))
			   ;; The second interval covers all of the
			   ;; first interval.
			   nil)
			  ((ratio-lower-bound-<= l2 l1)
			   ;; The upper bound of the second interval
			   ;; is necessarily less than the upper bound
			   ;; of the third interval, so the upper
			   ;; bound of the second interval is either a
			   ;; number or a CONS.
			   `(t
			     ,(if (consp u2) (car u2) (list u2))
			     ,u1))
			  ((ratio-upper-bound->= u2 u1)
			   ;; The lower bound of the second interval
			   ;; is necessarily greater than the lower
			   ;; bound of the third interval, so the
			   ;; lower bound of the second interval is
			   ;; either a number or a CONS.
			   `(t
			     ,l1
			     ,(if (consp l2) (car l2) (list l2))))
			  (t
			   ;; We are trying to remove an interval in
			   ;; the middle of a wider interval.  We give
			   ;; up and just allow the entire original
			   ;; interval.
			   descriptor1)))
		   (t
		    ;; The first interval is an integer interval and
		    ;; the second interval is a ratio interval.  We
		    ;; handle this situation by converting the second
		    ;; interval to an integer interval and calling the
		    ;; function recursively.
		    (rational-diff
		     descriptor1
		     `(nil
		       ,(cond ((numberp l2)
			       (ceiling l2))
			      ((consp l2)
			       (if (integerp (car l2))
				   (1+ (car l2))
				   (ceiling (car l2))))
			      (t
			       ;; l2 is *.
			       l2))
		       ,(cond ((numberp u2)
			       (floor u2))
			      ((consp u2)
			       (if (integerp (car u2))
				   (1- (car u2))
				   (floor (car u2))))
			      (t
			       ;; u2 is *.
			       u2)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Float descriptor.
;;;
;;; A float descriptor is either NIL, meaning floats are not allowed,
;;; or a list of two elements, the lower and the upper bound.

(defun float-or (descriptor1 descriptor2)
  (cond ((null descriptor1) descriptor2)
	((null descriptor2) descriptor1)
	(t
	 (list (non-integer-lower-bound-or
		(first descriptor1) (first descriptor2) )
	       (non-integer-upper-bound-or
		(second descriptor1) (second descriptor2))))))

(defun float-and (descriptor1 descriptor2)
  (if (or (null descriptor1) (null descriptor2))
      nil
      (list (lower-bound-and (first descriptor1) (first descriptor2) )
	    (upper-bound-and (second descriptor1) (second descriptor2)))))

(defun float-diff (descriptor1 descriptor2)
  (if (equal descriptor2 '(* *))
      nil
      descriptor1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Complex descriptor
;;;
;;; A complex descriptor is either NIL, meaning complex numers are not
;;; allowed, '*, or one of long-float, double-float, single-float,
;;; short-float, float, rational, integer, or fixnum.

(defun complex-or (descriptor1 descriptor2)
  (cond ((null descriptor1)
	 descriptor2)
	((null descriptor2)
	 descriptor1)
	((eq descriptor1 descriptor2)
	 descriptor1)
	((and (member descriptor1
		      '(long-float double-float single-float short-float float))
	      (member descriptor2
		      '(long-float double-float single-float short-float float)))
	 'float)
	((or (member descriptor1
		     '(long-float double-float single-float short-float float))
	     (member descriptor2
		     '(long-float double-float single-float short-float float)))
	 '*)
	((or (eq descriptor1 'rational) (eq descriptor2 'rational))
	 'rational)
	((or (eq descriptor1 'integer) (eq descriptor2 'integer))
	 'integer)
	(t
	 'fixnum)))

(defun complex-and (descriptor1 descriptor2)
  (cond ((or (null descriptor1) (null descriptor2))
	 'nil)
	((eq descriptor1 descriptor2)
	 descriptor1)
	((eq descriptor1 '*)
	 descriptor2)
	((eq descriptor2 '*)
	 descriptor1)
	((and (eq descriptor1 'float)
	      (member descriptor2
		      '(long-float double-float single-float short-float)))
	 descriptor2)
	((and (eq descriptor2 'float)
	      (member descriptor1
		      '(long-float double-float single-float short-float)))
	 descriptor1)
	((and (eq descriptor1 'rational)
	      (member descriptor2
		      '(integer fixnum)))
	 descriptor2)
	((and (eq descriptor2 'rational)
	      (member descriptor1
		      '(integer fixnum)))
	 descriptor1)
	((or (and (eq descriptor1 'integer) (eq descriptor2 'fixnum))
	     (and (eq descriptor2 'integer) (eq descriptor1 'fixnum)))
	 'fixnum)
	(t
	 nil)))

(defun complex-diff (descriptor1 descriptor2)
  (cond ((eq descriptor1 descriptor2)
	 nil)
	((eq descriptor2 '*)
	 nil)
	((and (eq descriptor2 'float)
	      (member descriptor1
		      '(long-float double-float single-float short-float)))
	 nil)
	((and (eq descriptor2 'rational)
	      (member descriptor1 '(integer 'fixnum)))
	 nil)
	((and (eq descriptor2 'integer) (eq descriptor1 'fixnum))
	 nil)
	(t
	 descriptor1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Character descriptor
;;;
;;; A character descriptor is either NIL, meaning characters are not
;;; allowed, or T meaning characters are allowed.

(defun character-or (descriptor1 descriptor2)
  (or descriptor1 descriptor2))

(defun character-and (descriptor1 descriptor2)
  (and descriptor1 descriptor2))

(defun character-diff (descriptor1 descriptor2)
  (if (null descriptor2)
      descriptor1
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cons descriptor
;;;
;;; A cons descriptor is either NIL, meaning conses are not
;;; allowed, or T meaning conses are allowed.

(defun cons-or (descriptor1 descriptor2)
  (or descriptor1 descriptor2))

(defun cons-and (descriptor1 descriptor2)
  (and descriptor1 descriptor2))

(defun cons-diff (descriptor1 descriptor2)
  (if (null descriptor2)
      descriptor1
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Array descriptor
;;;
;;; An array descriptor is either NIL (meaning arrays are not
;;; allowed), * (meaning any array of any rank is allowed), a
;;; non-negative integer (meaning arrays of that rank are allowed), or
;;; a list of dimensions.  A list of all * is canonicalized to the
;;; length of that list.  Otherwise, a dimension is either a
;;; non-negative integer or *.

(defun array-or (descriptor1 descriptor2)
  (cond ((null descriptor1)
	 descriptor2)
	((null descriptor2)
	 descriptor1)
	((equal descriptor1 descriptor2)
	 descriptor1)
	((or (eq descriptor1 '*) (eq descriptor2 '*))
	 '*)
	((numberp descriptor1)
	 (cond ((numberp descriptor2)
		;; Since they are not the same, upgrade.
		'*)
	       ((= (length descriptor2) descriptor1)
		descriptor1)
	       (t
		'*)))
	((numberp descriptor2)
	 (cond ((numberp descriptor1)
		;; Since they are not the same, upgrade.
		'*)
	       ((= (length descriptor1) descriptor2)
		descriptor2)
	       (t
		'*)))
	((not (= (length descriptor1) (length descriptor2)))
	 '*)
	(t
	 ;; Both are lists of the same length
	 (let ((result
		 (loop for d1 in descriptor1
		       for d2 in descriptor2
		       collect (if (eql d1 d2)
				   d1
				   '*))))
	   (if (every (lambda (x) (eq x '*)) result)
	       ;; Canonicalize.
	       (length result)
	       result)))))
	       
(defun array-and (descriptor1 descriptor2)
  (cond ((or (null descriptor1) (null descriptor2))
	 nil)
	((equal descriptor1 descriptor2)
	 descriptor1)
	((eq descriptor1 '*)
	 descriptor2)
	((eq descriptor2 '*)
	 descriptor1)
	((numberp descriptor1)
	 (if (or (numberp descriptor2)
		 (not (= (length descriptor2) descriptor1)))
	     nil
	     descriptor2))
	((numberp descriptor2)
	 (if (or (numberp descriptor1)
		 (not (= (length descriptor1) descriptor2)))
	     nil
	     descriptor1))
	((not (= (length descriptor1) (length descriptor2)))
	 nil)
	(t
	 (loop for d1 in descriptor1
	       for d2 in descriptor2
	       collect (cond ((and (numberp d1) (numberp d2))
			      (return-from array-and nil))
			     ((numberp d1)
			      d1)
			     ((numberp d2)
			      d2)
			     (t
			      '*))))))

(defun array-diff (descriptor1 descriptor2)
  (cond ((eq descriptor2 '*)
	 nil)
	((numberp descriptor2)
	 (if (or (eql descriptor1 descriptor2)
		 (and (listp descriptor1)
		      (= (length descriptor1) descriptor2)))
	     nil
	     descriptor1))
	((or (eq descriptor1 '*) (numberp descriptor1))
	 descriptor1)
	((not (= (length descriptor1) (length descriptor2)))
	 descriptor1)
	(t
	 (if (every (lambda (d1 d2)
		      (or (eq d2 '*) (eql d1 d2)))
		    descriptor1 descriptor2)
	     nil
	     descriptor1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NULL descriptor.
;;;
;;; The NULL descriptor is T if NULL is an allowed type, otherwise it
;;; is NIL.

(defun null-or (descriptor1 descriptor2)
  (or descriptor1 descriptor2))

(defun null-and (descriptor1 descriptor2)
  (and descriptor1 descriptor2))

(defun null-diff (descriptor1 descriptor2)
  (if (null descriptor2)
      descriptor1
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Others descriptor.
;;;
;;; The others descriptor is either NIL (meaning no other type is
;;; allowed), * (meaning any other type is allowed), or a list of
;;; other allowed atomic types.

(defun others-or (descriptor1 descriptor2)
  (cond ((null descriptor1)
	 descriptor2)
	((null descriptor2)
	 descriptor1)
	((or (eq descriptor1 '*) (eq descriptor2 '*))
	 '*)
	(t
	 (union descriptor1 descriptor2 :test #'eq))))

(defun others-and (descriptor1 descriptor2)
  (cond ((or (null descriptor1) (null descriptor2))
	 nil)
	((eq descriptor1 '*)
	 descriptor2)
	((eq descriptor2 '*)
	 descriptor1)
	(t
	 (intersection descriptor1 descriptor2 :test #'eq))))

(defun others-diff (descriptor1 descriptor2)
  (cond ((eq descriptor2 '*)
	 nil)
	((eq descriptor1 '*)
	 '*)
	(t
	 (set-difference descriptor1 descriptor2
			 :test #'eq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type descriptor.

(defclass type-descriptor ()
  ((%rational-descriptor
    :initform nil
    :initarg :rational-descriptor
    :reader rational-descriptor)
   (%short-float-descriptor
    :initform nil
    :initarg :short-float-descriptor
    :reader short-float-descriptor)
   (%single-float-descriptor
    :initform nil
    :initarg :single-float-descriptor
    :reader single-float-descriptor)
   (%double-float-descriptor
    :initform nil
    :initarg :double-float-descriptor
    :reader double-float-descriptor)
   (%long-float-descriptor
    :initform nil
    :initarg :long-float-descriptor
    :reader long-float-descriptor)
   (%complex-descriptor
    :initform nil
    :initarg :complex-descriptor
    :reader complex-descriptor)
   (%character-descriptor
    :initform nil
    :initarg :character-descriptor
    :reader character-descriptor)
   (%cons-descriptor
    :initform nil
    :initarg :cons-descriptor
    :reader cons-descriptor)
   (%array-t-descriptor
    :initform nil
    :initarg :array-t-descriptor
    :reader array-t-descriptor)
   (%array-single-float-descriptor
    :initform nil
    :initarg :array-single-float-descriptor
    :reader array-single-float-descriptor)
   (%array-double-float-descriptor
    :initform nil
    :initarg :array-double-float-descriptor
    :reader array-double-float-descriptor)
   (%array-character-descriptor
    :initform nil
    :initarg :array-character-descriptor
    :reader array-character-descriptor)
   (%array-bit-descriptor
    :initform nil
    :initarg :array-bit-descriptor
    :reader array-bit-descriptor)
   (%array-unsigned-byte-8-descriptor
    :initform nil
    :initarg :array-unsigned-byte-8-descriptor
    :reader array-unsigned-byte-8-descriptor)
   (%array-unsigned-byte-32-descriptor
    :initform nil
    :initarg :array-unsigned-byte-32-descriptor
    :reader array-unsigned-byte-32-descriptor)
   (%array-signed-byte-32-descriptor
    :initform nil
    :initarg :array-signed-byte-32-descriptor
    :reader array-signed-byte-32-descriptor)
   (%array-unsigned-byte-64-descriptor
    :initform nil
    :initarg :array-unsigned-byte-64-descriptor
    :reader array-unsigned-byte-64-descriptor)
   (%array-signed-byte-64-descriptor
    :initform nil
    :initarg :array-signed-byte-64-descriptor
    :reader array-signed-byte-64-descriptor)
   (%null-descriptor
    :initform nil
    :initarg :null-descriptor
    :reader null-descriptor)
   (%others-descriptor
    :initform nil
    :initarg :others-descriptor
    :reader others-descriptor)))

(defun type-descriptor-or (descriptor1 descriptor2)
  (make-instance 'type-descriptor
    :rational-descriptor (rational-or (rational-descriptor descriptor1)
			              (rational-descriptor descriptor1))
    :short-float-descriptor (float-or (short-float-descriptor descriptor1)
			              (short-float-descriptor descriptor1))
    :single-float-descriptor (float-or (short-float-descriptor descriptor1)
	 		               (short-float-descriptor descriptor1))
    :double-float-descriptor (float-or (short-float-descriptor descriptor1)
			               (short-float-descriptor descriptor1))
    :long-float-descriptor (float-or (short-float-descriptor descriptor1)
			             (short-float-descriptor descriptor1))
    :complex-descriptor (complex-or (complex-descriptor descriptor1)
			            (complex-descriptor descriptor2))
    :character-descriptor (character-or (character-descriptor descriptor1)
			                (character-descriptor descriptor2))
    :cons-descriptor (or (cons-descriptor descriptor1)
		         (cons-descriptor descriptor2))
    :array-double-float-descriptor
    (array-or (array-double-float-descriptor descriptor1)
              (array-double-float-descriptor descriptor2))
    :array-single-float-descriptor
    (array-or (array-single-float-descriptor descriptor1)
              (array-single-float-descriptor descriptor2))
    :array-character-descriptor
    (array-or (array-character-descriptor descriptor1)
              (array-character-descriptor descriptor2))
    :array-bit-descriptor (array-or (array-bit-descriptor descriptor1)
			            (array-bit-descriptor descriptor2))
    :array-unsigned-byte-8-descriptor
    (array-or (array-unsigned-byte-8-descriptor descriptor1)
              (array-unsigned-byte-8-descriptor descriptor2))
    :array-unsigned-byte-32-descriptor
    (array-or (array-unsigned-byte-32-descriptor descriptor1)
              (array-unsigned-byte-32-descriptor descriptor2))
    :array-signed-byte-32-descriptor
    (array-or (array-signed-byte-32-descriptor descriptor1)
              (array-signed-byte-32-descriptor descriptor2))
    :array-unsigned-byte-64-descriptor
    (array-or (array-unsigned-byte-64-descriptor descriptor1)
              (array-unsigned-byte-64-descriptor descriptor2))
    :array-signed-byte-64-descriptor
    (array-or (array-signed-byte-64-descriptor descriptor1)
              (array-signed-byte-64-descriptor descriptor2))
    :null-descriptor (null-or (null-descriptor descriptor1)
		              (null-descriptor descriptor2))

    :others-descriptor (others-or (others-descriptor descriptor1)
			          (others-descriptor descriptor2))))

(defun type-descriptor-and (descriptor1 descriptor2)
  (make-instance 'type-descriptor
    :rational-descriptor (rational-and (rational-descriptor descriptor1)
			               (rational-descriptor descriptor1))
    :short-float-descriptor (float-and (short-float-descriptor descriptor1)
			               (short-float-descriptor descriptor1))
    :single-float-descriptor (float-and (short-float-descriptor descriptor1)
	 		                (short-float-descriptor descriptor1))
    :double-float-descriptor (float-and (short-float-descriptor descriptor1)
			                (short-float-descriptor descriptor1))
    :long-float-descriptor (float-and (short-float-descriptor descriptor1)
			              (short-float-descriptor descriptor1))
    :complex-descriptor (complex-and (complex-descriptor descriptor1)
			             (complex-descriptor descriptor2))
    :character-descriptor (character-and (character-descriptor descriptor1)
			                 (character-descriptor descriptor2))
    :cons-descriptor (and (cons-descriptor descriptor1)
		          (cons-descriptor descriptor2))
    :array-character-descriptor
    (array-and (array-character-descriptor descriptor1)
               (array-character-descriptor descriptor2))
    :array-bit-descriptor (array-and (array-bit-descriptor descriptor1)
			             (array-bit-descriptor descriptor2))
    :array-unsigned-byte-8-descriptor
    (array-and (array-unsigned-byte-8-descriptor descriptor1)
               (array-unsigned-byte-8-descriptor descriptor2))
    :array-unsigned-byte-32-descriptor
    (array-and (array-unsigned-byte-32-descriptor descriptor1)
               (array-unsigned-byte-32-descriptor descriptor2))
    :array-signed-byte-32-descriptor
    (array-and (array-signed-byte-32-descriptor descriptor1)
               (array-signed-byte-32-descriptor descriptor2))
    :array-unsigned-byte-64-descriptor
    (array-and (array-unsigned-byte-64-descriptor descriptor1)
               (array-unsigned-byte-64-descriptor descriptor2))
    :array-signed-byte-64-descriptor
    (array-and (array-signed-byte-64-descriptor descriptor1)
               (array-signed-byte-64-descriptor descriptor2))
    :array-double-float-descriptor
    (array-and (array-double-float-descriptor descriptor1)
               (array-double-float-descriptor descriptor2))
    :array-single-float-descriptor
    (array-and (array-single-float-descriptor descriptor1)
               (array-single-float-descriptor descriptor2))
    :null-descriptor (null-and (null-descriptor descriptor1)
		               (null-descriptor descriptor2))

    :others-descriptor (others-and (others-descriptor descriptor1)
			           (others-descriptor descriptor2))))

(defun type-descriptor-diff (descriptor1 descriptor2)
  (make-instance 'type-descriptor
    :rational-descriptor (rational-diff (rational-descriptor descriptor1)
			                (rational-descriptor descriptor1))
    :short-float-descriptor (float-diff (short-float-descriptor descriptor1)
			                (short-float-descriptor descriptor1))
    :single-float-descriptor (float-diff (short-float-descriptor descriptor1)
	 		                 (short-float-descriptor descriptor1))
    :double-float-descriptor (float-diff (short-float-descriptor descriptor1)
			                 (short-float-descriptor descriptor1))
    :long-float-descriptor (float-diff (short-float-descriptor descriptor1)
			               (short-float-descriptor descriptor1))
    :complex-descriptor (complex-diff (complex-descriptor descriptor1)
			              (complex-descriptor descriptor2))
    :character-descriptor (character-diff (character-descriptor descriptor1)
			                  (character-descriptor descriptor2))
    :cons-descriptor (and (cons-descriptor descriptor1)
		          (not (cons-descriptor descriptor2)))
    :array-character-descriptor
    (array-diff (array-character-descriptor descriptor1)
                (array-character-descriptor descriptor2))
    :array-bit-descriptor (array-diff (array-bit-descriptor descriptor1)
			              (array-bit-descriptor descriptor2))
    :array-unsigned-byte-8-descriptor
    (array-diff (array-unsigned-byte-8-descriptor descriptor1)
                (array-unsigned-byte-8-descriptor descriptor2))
    :array-unsigned-byte-32-descriptor
    (array-diff (array-unsigned-byte-32-descriptor descriptor1)
                (array-unsigned-byte-32-descriptor descriptor2))
    :array-signed-byte-32-descriptor
    (array-diff (array-signed-byte-32-descriptor descriptor1)
                (array-signed-byte-32-descriptor descriptor2))
    :array-unsigned-byte-64-descriptor
    (array-diff (array-unsigned-byte-64-descriptor descriptor1)
                (array-unsigned-byte-64-descriptor descriptor2))
    :array-signed-byte-64-descriptor
    (array-diff (array-signed-byte-64-descriptor descriptor1)
                (array-signed-byte-64-descriptor descriptor2))
    :array-double-float-descriptor
    (array-diff (array-double-float-descriptor descriptor1)
                (array-double-float-descriptor descriptor2))
    :array-single-float-descriptor
    (array-diff (array-single-float-descriptor descriptor1)
                (array-single-float-descriptor descriptor2))
    :null-descriptor (null-diff (null-descriptor descriptor1)
		                (null-descriptor descriptor2))

    :others-descriptor (others-diff (others-descriptor descriptor1)
			            (others-descriptor descriptor2))))

(defun type-descriptor-equal (type-descriptor-1 type-descriptor-2)
  (and (equal (rational-descriptor type-descriptor-1)
	      (rational-descriptor type-descriptor-2))
       (equal (short-float-descriptor type-descriptor-1)
	      (short-float-descriptor type-descriptor-2))
       (equal (single-float-descriptor type-descriptor-1)
	      (single-float-descriptor type-descriptor-2))
       (equal (double-float-descriptor type-descriptor-1)
	      (double-float-descriptor type-descriptor-2))
       (equal (long-float-descriptor type-descriptor-1)
	      (long-float-descriptor type-descriptor-2))
       (equal (complex-descriptor type-descriptor-1)
	      (complex-descriptor type-descriptor-2))
       (equal (character-descriptor type-descriptor-1)
	      (character-descriptor type-descriptor-2))
       (equal (cons-descriptor type-descriptor-1)
	      (cons-descriptor type-descriptor-2))
       (equal (array-t-descriptor type-descriptor-1)
	      (array-t-descriptor type-descriptor-2))
       (equal (array-single-float-descriptor type-descriptor-1)
	      (array-single-float-descriptor type-descriptor-2))
       (equal (array-double-float-descriptor type-descriptor-1)
	      (array-double-float-descriptor type-descriptor-2))
       (equal (array-character-descriptor type-descriptor-1)
	      (array-character-descriptor type-descriptor-2))
       (equal (array-bit-descriptor type-descriptor-1)
	      (array-bit-descriptor type-descriptor-2))
       (equal (array-unsigned-byte-8-descriptor type-descriptor-1)
	      (array-unsigned-byte-8-descriptor type-descriptor-2))
       (equal (array-unsigned-byte-32-descriptor type-descriptor-1)
	      (array-unsigned-byte-32-descriptor type-descriptor-2))
       (equal (array-signed-byte-32-descriptor type-descriptor-1)
	      (array-signed-byte-32-descriptor type-descriptor-2))
       (equal (array-unsigned-byte-64-descriptor type-descriptor-1)
	      (array-unsigned-byte-64-descriptor type-descriptor-2))
       (equal (array-signed-byte-64-descriptor type-descriptor-1)
	      (array-signed-byte-64-descriptor type-descriptor-2))
       (equal (null-descriptor type-descriptor-1)
	      (null-descriptor type-descriptor-2))
       (or (and (eq (others-descriptor type-descriptor-1) '*)
		(eq (others-descriptor type-descriptor-2) '*))
	   (and (not (eq (others-descriptor type-descriptor-1) '*))
		(not (eq (others-descriptor type-descriptor-2) '*))
		(null (set-exclusive-or
		       (others-descriptor type-descriptor-1)
		       (others-descriptor type-descriptor-1)))))))
       

(defun make-t-type-descriptor ()
  (make-instance 'type-descriptor
    :rational-descriptor '(t * *)
    :short-float-descriptor '(* *)
    :single-float-descriptor '(* *)
    :double-float-descriptor '(* *)
    :long-float-descriptor '(* *)
    :complex-descriptor '(*)
    :character-descriptor t
    :cons-descriptor t
    :array-character-descriptor '*
    :array-t-descriptor '*
    :array-single-float-descriptor '*
    :array-double-float-descriptor '*
    :array-bit-descriptor '*
    :array-unsigned-byte-8-descriptor '*
    :array-unsigned-byte-32-descriptor '*
    :array-signed-byte-32-descriptor '*
    :array-unsigned-byte-64-descriptor '*
    :array-signed-byte-64-descriptor '*
    :null-descriptor t
    :others-descriptor '*))

(defgeneric make-type-descriptor (symbol rest))

(defun type-descriptor-from-type (type)
  (if (symbolp type)
      (case type
	(nil
	 (make-instance 'type-descriptor))
	(null
	 (make-instance 'type-descriptor
	   :null-descriptor t))
	(character
	 (make-instance 'type-descriptor
	   :character-descriptor t))
	;; FIXME: make this depend on the backend
	(fixnum (type-descriptor-from-type
		 '(integer #.(- (expt 2 29)) #.(1- (expt 2 29)))))
	;; FIXME: make this depend on the backend
	(bignum (type-descriptor-from-type
		 '(or
		   (integer * #.(1- (- (expt 2 29))))
		   (integer #.(expt 2 29) *))))
	(bit (type-descriptor-from-type '(integer 0 1)))
	(integer (type-descriptor-from-type '(integer * *)))
	(short-float (type-descriptor-from-type '(short-float * *)))
	(single-float (type-descriptor-from-type '(single-float * *)))
	(double-float (type-descriptor-from-type '(double-float * *)))
	(long-float (type-descriptor-from-type '(long-float * *)))
	(float (type-descriptor-from-type
		'(or
		  (short-float '(short-float * *))
		  (single-float '(single-float * *))
		  (double-float '(double-float * *))
		  (long-float '(long-float * *)))))
	(ratio (type-descriptor-from-type
		'(and
		  (rational * *)
		  (not (integer * *)))))
	(rational (type-descriptor-from-type '(rational * *)))
	(complex (type-descriptor-from-type '(complex *)))
	(number (type-descriptor-from-type
		 '(or
		   (rational * *)
		   (short-float '(short-float * *))
		   (single-float '(single-float * *))
		   (double-float '(double-float * *))
		   (long-float '(long-float * *))
		   (complex *))))
	(cons (type-descriptor-from-type '(cons * *)))
	(list (type-descriptor-from-type '(or (cons * *) null)))
	(array (type-descriptor-from-type '(array * *)))
	(vector (type-descriptor-from-type '(array * 1)))
	(string (type-descriptor-from-type '(array character 1)))
	(t (make-instance 'type-descriptor
	     :others-descriptor (list type))))
      (make-type-descriptor (car type) (cdr type))))

(defmethod make-type-descriptor ((symbol (eql 'mod)) rest)
  (type-descriptor-from-type
   `(integer 0 (,(car rest)))))

(defmethod make-type-descriptor ((symbol (eql 'signed-byte)) rest)
  (type-descriptor-from-type
   (if (eq (car rest) '*)
       'integer
       `(integer ,(- (expt 2 (1- (car rest))))
		 ,(1- (expt 2 (1- (car rest))))))))

(defmethod make-type-descriptor ((symbol (eql 'unsigned-byte)) rest)
  (type-descriptor-from-type
   (if (eq (car rest) '*)
       '(integer 0 *)
       `(mod ,(expt 2 (car rest))))))

(defmethod make-type-descriptor ((symbol (eql 'integer)) rest)
  (make-instance 'type-descriptor
    :rational-descriptor (cons nil rest)))

(defmethod make-type-descriptor ((symbol (eql 'rational)) rest)
  (make-instance 'type-descriptor
    :rational-descriptor (cons t rest)))

(defmethod make-type-descriptor ((symbol (eql 'float)) rest)
  (make-instance 'type-descriptor
    :short-float rest
    :single-float rest
    :double-float rest
    :long-float rest))

(defmethod make-type-descriptor ((symbol (eql 'short-float)) rest)
  (make-instance 'type-descriptor
    :short-float rest))

(defmethod make-type-descriptor ((symbol (eql 'single-float)) rest)
  (make-instance 'type-descriptor
    :single-float rest))

(defmethod make-type-descriptor ((symbol (eql 'double-float)) rest)
  (make-instance 'type-descriptor
    :double-float rest))

(defmethod make-type-descriptor ((symbol (eql 'long-float)) rest)
  (make-instance 'type-descriptor
    :long-float rest))

(defmethod make-type-descriptor ((symbol (eql 'real)) rest)
  (make-instance 'type-descriptor  
    :rational-descriptor (cons t rest)
    :short-float rest
    :single-float rest
    :double-float rest
    :long-float rest))

(defmethod make-type-descriptor ((symbol (eql 'complex)) rest)
  (make-instance 'type-descriptor
    :complex (car rest)))

(defmethod make-type-descriptor ((symbol (eql 'array)) rest)
  (cond ((eq (car rest) '*)
	 (make-instance 'type-descriptor
	   :array-t-descriptor (cdr rest)
	   :array-single-float-descriptor (cdr rest)
	   :array-double-float-descriptor (cdr rest)
	   :array-character-descriptor (cdr rest)
	   :array-bit-descriptor (cdr rest)
	   :array-unsigned-byte-8-descriptor (cdr rest)
	   :array-unsigned-byte-32-descriptor (cdr rest)
	   :array-signed-byte-32-descriptor (cdr rest)
	   :array-unsigned-byte-64-descriptor (cdr rest)
	   :array-signed-byte-64-descriptor (cdr rest)))
	((eq (car rest) 'character)
	 (make-instance 'type-descriptor
	   :array-character-descriptor (cdr rest)))
	((symbolp (car rest))
	 (make-instance 'type-descriptor
	   :array-t-descriptor (cdr rest)))
	(t
	 (case (caar rest)
	   (integer
	    (destructuring-bind (lower upper) (cdar rest)
	      (cond ((or (eq lower '*) (eq upper '*))
		     (make-instance 'type-descriptor
		       :array-t-descriptor (cdr rest)))
		    ((and (= lower 0) (= upper 1))
		     (make-instance 'type-descriptor
		       :array-bit-descriptor (cdr rest)))
		    ((and (>= lower 0) (< upper (expt 2 8)))
		     (make-instance 'type-descriptor
		       :array-unsigned-byte-8-descriptor (cdr rest)))
		    ((and (>= lower 0) (< upper (expt 2 32)))
		     (make-instance 'type-descriptor
		       :array-unsigned-byte-32-descriptor (cdr rest)))
		    ((and (>= lower (- (expt 2 31))) (< upper (expt 2 31)))
		     (make-instance 'type-descriptor
		       :array-signed-byte-32-descriptor (cdr rest)))
		    ((and (>= lower 0) (< upper (expt 2 64)))
		     (make-instance 'type-descriptor
		       :array-unsigned-byte-64-descriptor (cdr rest)))
		    ((and (>= lower (- (expt 2 63))) (< upper (expt 2 63)))
		     (make-instance 'type-descriptor
		       :array-signed-byte-64-descriptor (cdr rest)))
		    (t
		     (make-instance 'type-descriptor
		       :array-t-descriptor (cdr rest))))))
	   (float
	    (make-instance 'type-descriptor
	      :array-single-float-descriptor (cdr rest)
	      :array-double-float-descriptor (cdr rest)))
	   (single-float
	    (make-instance 'type-descriptor
	      :array-single-float-descriptor (cdr rest)))
	   (double-float
	    (make-instance 'type-descriptor
	      :array-double-float-descriptor (cdr rest)))))))

(defmethod make-type-descriptor ((symbol (eql 'string)) rest)
  (type-descriptor-from-type
   (if (or (null rest) (eq (car rest) '*))
       '(array character)
       `(array character (,(car rest))))))

(defmethod make-type-descriptor ((symbol (eql 'vector)) rest)
  (type-descriptor-from-type
   (if (or (null rest) (equal rest '(*)))
       '(array *)
       `(array ,(car rest) (,(cadr rest))))))

(defmethod make-type-descriptor ((symbol (eql 'or)) rest)
  (reduce #'type-descriptor-or
	  (mapcar #'type-descriptor-from-type rest)))

(defmethod make-type-descriptor ((symbol (eql 'and)) rest)
  (reduce #'type-descriptor-and
	  (mapcar #'type-descriptor-from-type rest)))

(defmethod make-type-descriptor ((symbol (eql 'not)) rest)
  (type-descriptor-diff (make-t-type-descriptor)
			(type-descriptor-from-type (car rest))))

(defmethod make-type-descriptor ((symbol (eql 'satisfies)) rest)
  (make-t-type-descriptor))

;;; FIXME: do a better job here.
(defmethod make-type-descriptor ((symbol (eql 'eql)) rest)
  (make-t-type-descriptor))

;;; FIXME: do a better job here.
(defmethod make-type-descriptor ((symbol (eql 'member)) rest)
  (make-t-type-descriptor))

;;; Return true if the argument is a type descriptor with all the
;;; slots equal to NIL.

(defun nil-type-descriptor-p (type-descriptor)
  (and (null (rational-descriptor type-descriptor))
       (null (short-float-descriptor type-descriptor))
       (null (single-float-descriptor type-descriptor))
       (null (double-float-descriptor type-descriptor))
       (null (long-float-descriptor type-descriptor))
       (null (complex-descriptor type-descriptor))
       (null (character-descriptor type-descriptor))
       (null (cons-descriptor type-descriptor))
       (null (array-t-descriptor type-descriptor))
       (null (array-single-float-descriptor type-descriptor))
       (null (array-double-float-descriptor type-descriptor))
       (null (array-character-descriptor type-descriptor))
       (null (array-bit-descriptor type-descriptor))
       (null (array-unsigned-byte-8-descriptor type-descriptor))
       (null (array-unsigned-byte-32-descriptor type-descriptor))
       (null (array-signed-byte-32-descriptor type-descriptor))
       (null (array-unsigned-byte-64-descriptor type-descriptor))
       (null (array-signed-byte-64-descriptor type-descriptor))
       (null (null-descriptor type-descriptor))
       (null (others-descriptor type-descriptor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions and classes for type maps.
;;;
;;; A type map is a dictionary mapping variables to type descriptors.
;;; We represent a type map as an EQ hash table.  Client code can use
;;; whatever object it wants to represent variables, as long as those
;;; objects can be compared using EQ.
;;;
;;; Only variables whose types are something other than T need
;;; acutally be present in the hash table.  

(defun type-descriptor (variable type-map)
  (gethash variable type-map (make-t-type-descriptor)))

(defun (setf type-descriptor) (new-descriptor variable type-map)
  (check-type new-descriptor type-descriptor)
  (setf (gethash variable type-map) new-descriptor))

(defun type-map-or (type-map-1 type-map-2)
  (let ((result (make-hash-table :test #'eq)))
    (maphash (lambda (key descriptor1)
	       (let ((descriptor2 (gethash key type-map-2)))
		 (unless (null descriptor2)
		   (setf (gethash key result)
			 (type-descriptor-or descriptor1 descriptor2)))))
	     type-map-1)
    result))

(defun type-map-and (type-map-1 type-map-2)
  (let ((result (make-hash-table :test #'eq)))
    (maphash (lambda (key descriptor1)
	       (let ((descriptor2 (gethash key type-map-2)))
		 (setf (gethash key result)
		       (if (null descriptor2)
			   descriptor1
			   (type-descriptor-and descriptor1 descriptor2)))))
	     type-map-1)
    (maphash (lambda (key descriptor2)
	       (let ((descriptor1 (gethash key type-map-1)))
		 (when (null descriptor1)
		   (setf (gethash key result)
			 descriptor2))))
	     type-map-2)
    result))
    
(defun type-map-equal (type-map-1 type-map-2)
  (maphash (lambda (var descriptor1)
	     (let ((descriptor2 (gethash var type-map-2)))
	       (if (null descriptor2)
		   (unless (type-descriptor-equal
			    descriptor1 (make-t-type-descriptor))
		     (return-from type-map-equal nil))
		   (unless (type-descriptor-equal
			    descriptor1 descriptor2)
		     (return-from type-map-equal nil)))))
	   type-map-1)
  (maphash (lambda (var descriptor2)
	     (let ((descriptor1 (gethash var type-map-1)))
	       (when (null descriptor1)
		 (unless (type-descriptor-equal
			  descriptor2 (make-t-type-descriptor))
		   (return-from type-map-equal nil)))))
	   type-map-2))
  
(defun copy-type-map (type-map)
  (let ((result (make-hash-table :test #'eq)))
    (maphash (lambda (variable type-descriptor)
	       (setf (gethash variable result) type-descriptor))
	     type-map)
    result))

(defun split-type-map (type-map variable type-specifier)
  (let ((result1 (make-hash-table :test #'eq))
	(result2 (make-hash-table :test #'eq))
	(type-descriptor (type-descriptor-from-type type-specifier)))
    ;; Copy entries in TYPE-MAP to RESULT1 and RESULT2.
    (maphash (lambda (variable type-descriptor)
	       (setf (gethash variable result1) type-descriptor)
	       (setf (gethash variable result2) type-descriptor))
	     type-map)
    ;; Modify entry for VARIABLE in RESULT1
    (setf (gethash variable result1)
	  (type-descriptor-diff (sicl-compiler-types:type-descriptor
				 variable result1)
				type-descriptor))
    ;; Modify entry for VARIABLE in RESULT2
    (setf (gethash variable result2)
	  (type-descriptor-and (sicl-compiler-types:type-descriptor
				variable result2)
			       type-descriptor))
    (values result1 result2)))

;;; Make a type map in which all variables have type T.
(defun make-t-type-map ()
  (make-hash-table :test #'eq))

;;; Return true if a type map is "impossible", i.e., it contains a
;;; type descriptor describing the type NIL.

(defun impossible-type-map-p (type-map)
  (maphash (lambda (variable type-descriptor)
	     (declare (ignore variable))
	     (when (nil-type-descriptor-p type-descriptor)
	       (return-from impossible-type-map-p t)))
	   type-map)
  nil)
	   
