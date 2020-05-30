(cl:in-package #:sicl-type)

(defmethod typep-compound (object (head (eql 'integer)) rest)
  nil)

(defmethod typep-compound ((object integer) (head (eql 'integer)) rest)
  (when (null rest)
    ;; The type specifier is (INTEGER), so since OBJECT is an integer,
    ;; we are done.
    (return-from typep-compound t))
  ;; The type specifier is (INTEGER <lower-bound> . ...).
  (let ((lower-bound (first rest)))
    (cond ((integerp lower-bound)
	   (when (< object lower-bound)
	     (return-from typep-compound nil)))
	  ((consp lower-bound)
	   (when (<= object (car lower-bound))
	     (return-from typep-compound nil)))
	  (t
	   nil)))
  (when (null (rest rest))
    ;; The type specifier is (INTEGER <lower-bound>), so since we have
    ;; checked that the lower bound is OK, we are done.
    (return-from typep-compound t))
  (let ((upper-bound (second rest)))
    (cond ((integerp upper-bound)
	   (<= object upper-bound))
	  ((consp upper-bound)
	   (< object (car upper-bound)))
	  (t
	   t))))
