(cl:in-package #:sicl-type)

(defmethod typep-compound (object (head (eql 'rational)) rest)
  nil)

(defmethod typep-compound ((object rational) (head (eql 'rational)) rest)
  (when (null rest)
    ;; The type specifier is (RATIONAL), so since OBJECT is an rational,
    ;; we are done.
    (return-from typep-compound t))
  ;; The type specifier is (RATIONAL <lower-bound> . ...).
  (let ((lower-bound (first rest)))
    (cond ((rationalp lower-bound)
	   (when (< object lower-bound)
	     (return-from typep-compound nil)))
	  ((consp lower-bound)
	   (when (<= object (car lower-bound))
	     (return-from typep-compound nil)))
	  (t
	   nil)))
  (when (null (rest rest))
    ;; The type specifier is (RATIONAL <lower-bound>), so since we have
    ;; checked that the lower bound is OK, we are done.
    (return-from typep-compound t))
  (let ((upper-bound (second rest)))
    (cond ((rationalp upper-bound)
	   (<= object upper-bound))
	  ((consp upper-bound)
	   (< object (car upper-bound)))
	  (t
	   t))))
