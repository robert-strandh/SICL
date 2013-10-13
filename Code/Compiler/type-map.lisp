(in-package #:sicl-compiler-types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun rational-and (descriptor1 descriptor2)
  (cond ((or (null descriptor1) (null descriptor2))
	 nil)
	(t
	 (list (and (first descriptor1) (first descriptor2))
	       (lower-bound-and (second descriptor1) (second descriptor2))
	       (upper-bound-and (third descriptor1) (third descriptor2))))))

(defun rational-diff (descriptor1 descriptor2)
  (if (equal descriptor2 '(T * *))
      nil
      descriptor1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Float descriptor.
;;;
;;; A float descriptor is either NIL, meaning floats are not allowed,
;;; or a list of two elements, the lower and the upper bound.

(defun float-or (descriptor1 descriptor2)
  (list (non-integer-lower-bound-or (first descriptor1) (first descriptor2) )
	(non-integer-upper-bound-or (second descriptor1) (second descriptor2))))

(defun float-and (descriptor1 descriptor2)
  (list (lower-bound-and (first descriptor1) (first descriptor2) )
	(upper-bound-and (second descriptor1) (second descriptor2))))

(defun float-diff (descriptor1 descriptor2)
  (if (equal descriptor2 '(* *))
      nil
      descriptor1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Canonicalization.
;;;
;;; We assume that a type descriptor has been canonicalized:
;;;
;;;   * It no longer contains MOD, FIXNUM, or BYTE (signed or
;;;     unsigned).  They have been replaced by INTEGER.
;;;
;;;   * It no longer contains references to STRINGs or VECTORs.  They
;;;     have all been replaced by ARRAY.
;;;
;;;   * Abbreviated types have been expanded, so that for instanced 
;;;     INTEGER becomes (INTEGER * *).  
;;;
;;; The canonicalization is achieved by the use of DEFTYPE.

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
      (cond ((null type)
	     (make-instance 'type-descriptor
	       :null-descriptor t))
	    ((eq type 'character)
	     (make-instance 'type-descriptor
	       :character-descriptor t))
	    (t
	     (make-instance 'type-descriptor
	       :others-descriptor (list type))))
      (make-type-descriptor (car type) (cdr type))))

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
