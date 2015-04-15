(cl:in-package #:sicl-type)

;;; Determine the floating-point types of the implementation.  The
;;; following return values are possible:
;;;
;;;   * (SINGLE-FLOAT)
;;; 
;;;     This return value indicates that the implementation has a
;;;     single floating-point type.  The HyperSpec then requires this
;;;     type to be SINGLE-FLOAT, and it requires that all
;;;     floating-point types are the same.
;;;
;;;   * (SHORT-FLOAT SINGLE-FLOAT)
;;;
;;;     This return value indicates that the implementation has two
;;;     floating-point types, namely SHORT-FLOAT and SINGLE-FLOAT.
;;;     The HyperSpec then requires that the types DOUBLE-FLOAT and
;;;     LONG-FLOAT to be the same type as SINGLE-FLOAT.
;;;
;;;   * (SINGLE-FLOAT DOUBLE-FLOAT)
;;;
;;;     This return value indicates that the implementation has two
;;;     floating-point types, namely SINGLE-FLOAT and DOUBLE-FLOAT.
;;;     The HyperSpec then requires the type SHORT-FLOAT to be the
;;;     same as SINGLE-FLOAT, and the type LONG-FLOAT to be the same
;;;     as DOUBLE-FLOAT.
;;;
;;;   * (SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT)
;;;
;;;     This return value indicates that the implementation has three
;;;     floating-point types, namely SHORT-FLOAT, SINGLE-FLOAT, and
;;;     DOUBLE-FLOAT.  The HyperSpec then requires the type LONG-FLOAT
;;;     to be the same as DOUBLE-FLOAT.
;;;
;;;   * (SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT)
;;;
;;;     This return value indicates that the implementation has three
;;;     floating-point types, namely SINGLE-FLOAT, DOUBLE-FLOAT, and
;;;     LONG-FLOAT.  The HyperSpec then requires the type SHORT-FLOAT
;;;     to be the same as SINGLE-FLOAT.

(defun non-conforming ()
  (error "Non-conforming floating-point types of implementation."))

(defun floating-point-types ()
  (let ((short (coerce 1 'short-float))
	(single (coerce 1 'single-float))
	(double (coerce 1 'double-float))
	(long (coerce 1 'long-float)))
    (if (eql short single)
	(if (eql single double)
	    (if (eql double long)
		'(single-float)
		(non-conforming))
	    (if (eql double long)
		'(single-float double-float)
		'(single-float double-float long-float)))
	(if (eql single double)
	    (non-conforming)
	    (if (eql double long)
		'(short-float single-float double-float)
		'(short-float single-float double-float long-float))))))

;;; Return true if and only the two atomic floating-point types TYPE1
;;; and TYPE2 are the same.
(defun same-float-type-p (type1 type2)
  (or (eq type1 type2)
      (eql (coerce 1 type1) (coerce 1 type2))))
