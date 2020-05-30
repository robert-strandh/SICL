(cl:in-package #:sicl-type)

(defmethod typep-compound (object (head (eql 'and)) rest)
  (loop for type-spec in rest
	always (generic-typep object type-spec)))

(defmethod typep-compound (object (head (eql 'eql)) rest)
  (eql object (first rest)))

(defmethod typep-compound (object (head (eql 'member)) rest)
  (member object rest))

(defmethod typep-compound (object (head (eql 'not)) rest)
  (assert (= (length rest) 1))
  (not (generic-typep object (first rest))))

(defmethod typep-compound (object (head (eql 'or)) rest)
  (loop for type-spec in rest
	when (generic-typep object type-spec)
	  return t))

(defmethod typep-compound (object (head (eql 'satisfies)) rest)
  (assert (= (length rest) 1))
  (funcall (sicl-genv:fdefinition (first rest) (sicl-genv:global-environment))
	   object))

(defmethod typep-compound (object (head (eql 'complex)) rest)
  nil)

(defmethod typep-compound ((complex object) (head (eql 'complex)) rest)
  (when (null rest)
    ;; the type specifier is (COMPLEX), so since OBJECT is a complex, we
    ;; are done.
    (return-from typep-compound t))
  ;; the type specifier is (COMPLEX ...)
  (if (eq (first rest) '*)
      t
      (let ((type (upgraded-complex-part-type (first rest))))
	;; the type specifier is (COMPLEX <type>).  In order for TYPEP to
	;; return true, the element type of the complex must be the
	;; same as the result of upgrading <type>.
	(and (generic-typep (realpart object) type)
	     (generic-typep (imagpart object) type)))))

(defmethod typep-compound (object (head (eql 'cons)) rest)
  nil)

(defmethod typep-compound ((object cons) (head (eql 'cons)) rest)
  (when (null rest)
    ;; the type specifier is (CONS), so since OBJECT is a CONS, we
    ;; are done.
    (return-from typep-compound t))
  ;; the type specifier is (CONS <type> . ...)
  (unless (generic-typep (car object) (first rest))
    (return-from typep-compound nil))
  ;; The CAR of OBJECT is the right type.  Now check the CDR.
  (when (null (rest rest))
    ;; the type specifier is (CONS <type>), so since OBJECT is a CONS, and
    ;; the CAR is the right type, we are done.
    (return-from typep-compound t))
  ;; the type specifier is (CONS <type> <type>)
  (generic-typep (cdr object) (second rest)))

(defmethod typep-compound (object (head (eql 'integer)) rest)
  nil)

(defmethod typep-compound ((object integer) (head (eql 'integer)) rest)
  (when (null rest)
    ;; the type specifier is (INTEGER), so since OBJECT is an integer,
    ;; we are done.
    (return-from typep-compound t))
  ;; the type specifier is (INTEGER <lower-bound> . ...).
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
    ;; the type specifier is (INTEGER <lower-bound>), so since we have
    ;; checked that the lower bound is OK, we are done.
    (return-from typep-compound t))
  (let ((upper-bound (second rest)))
    (cond ((integerp upper-bound)
	   (<= object upper-bound))
	  ((consp upper-bound)
	   (< object (car upper-bound)))
	  (t
	   t))))

(defmethod typep-compound (object (head (eql 'rational)) rest)
  nil)

(defmethod typep-compound ((object rational) (head (eql 'rational)) rest)
  (when (null rest)
    ;; the type specifier is (RATIONAL), so since OBJECT is an rational,
    ;; we are done.
    (return-from typep-compound t))
  ;; the type specifier is (RATIONAL <lower-bound> . ...).
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
    ;; the type specifier is (RATIONAL <lower-bound>), so since we have
    ;; checked that the lower bound is OK, we are done.
    (return-from typep-compound t))
  (let ((upper-bound (second rest)))
    (cond ((rationalp upper-bound)
	   (<= object upper-bound))
	  ((consp upper-bound)
	   (< object (car upper-bound)))
	  (t
	   t))))

(defun typep-compound-float (object head rest)
  (unless (same-float-type-p (type-of object) head)
    (return-from typep-compound-float nil))
  ;; OBJECT is definitely a float of the right type.
  (when (null rest)
    ;; The type specifier is (...-FLOAT), so since OBJECT is a float
    ;; of the right type, we are done.
    (return-from typep-compound-float t))
  ;; The type specifier is (...-FLOAT <lower-bound> . ...).
  (let ((lower-bound (first rest)))
    (cond ((floatp lower-bound)
	   (when (< object lower-bound)
	     (return-from typep-compound-float nil)))
	  ((consp lower-bound)
	   (when (<= object (car lower-bound))
	     (return-from typep-compound-float nil)))
	  (t
	   nil)))
  (when (null (rest rest))
    ;; The type specifier is (...-FLOAT <lower-bound>), so since we
    ;; have checked that the lower bound is OK, we are done.
    (return-from typep-compound-float t))
  (let ((upper-bound (second rest)))
    (cond ((floatp upper-bound)
	   (<= object upper-bound))
	  ((consp upper-bound)
	   (< object (car upper-bound)))
	  (t
	   t))))
  
(defmethod typep-compound (object (head (eql 'short-float)) rest)
  nil)

(defmethod typep-compound (object (head (eql 'short-float)) rest)
  (typep-compound-float object head rest))

(defmethod typep-compound (object (head (eql 'single-float)) rest)
(defmethod typep-compound (object (head (eql 'single-float)) rest)
  (typep-compound-float object head rest))

(defmethod typep-compound ((object double-float) (head (eql 'double-float)) rest)
  (typep-compound-float object head rest))

(defmethod typep-compound (object (head (eql 'long-float)) rest)
  nil)

(defmethod typep-compound ((object double-float) (head (eql 'long-float)) rest)
  (typep-compound-float object head rest))
