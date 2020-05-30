(cl:in-package #:sicl-type)

(defun proper-list-p (object)
  (integerp (ignore-errors (list-length object))))

(defmethod typep-compound :before (object head rest)
  (assert (proper-list-p rest)))

;;; Given a type specifier that is illegal in the context of TYPEP,
;;; such as VALUES or FUNCTION, signal an error to that effect.
(defmethod typep-compound (object head rest)
  (declare (ignore object))
  (error "Compound type specifier is illegal for typep: ~s."
	 (cons head rest)))

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

(defmethod typep-compound :before (object (head (eql 'array)) rest)
  (assert (or (null rest)
              (let ((element-type (first rest)))
                ;; FIXME: check that the first element is a valid type
                ;; specifier or the symbol *.
                (declare (ignore element-type))
                (or (null (rest rest))
                    (let ((dimension-spec (second rest)))
                      (or (and (typep dimension-spec 'fixnum)
                               (not (minusp dimension-spec)))
                          (eq dimensions-spec '*)
                          (and (proper-list-p dimension-spec)
                               (loop for dimension in dimension-spec
                                     always (or (eq dimension '*)
                                                (and (typep dimension 'fixnum)
                                                     (not (minusp dimension-spec)))))))))))))

;;; Given two type specifiers, both indicating possible upgraded array
;;; element types, return true if and only if the two represent the
;;; same type.
(defun same-array-element-type-p (type-descriptor1 type-descriptor2)
  ;; If we are lucky, the two type descriptors are EQUAL.  In fact, I
  ;; can't imagine how an implementation would NOT make then EQUAL.
  (or (equal type-descriptor1 type-descriptor2)
      ;; If we are not lucky, we must ask SUBTYPEP, and then we insist
      ;; that it return true for the second return value in both
      ;; cases.
      (multiple-value-bind (subtype1-p valid1-p)
	  (subtypep type-descriptor1 type-descriptor2)
	(multiple-value-bind (subtype2-p valid2-p)
	    (subtypep type-descriptor2 type-descriptor1)
	  (assert (and valid1-p valid2-p))
	  (and subtype1-p subtype2-p)))))

(defmethod typep-compound (object (head (eql 'array)) rest)
  nil)

(defmethod typep-compound ((object array) (head (eql 'array)) rest)
  (when (null rest)
    ;; the type specifier is (ARRAY), so since OBJECT is an
    ;; array, we are done.
    (return-from typep-compound t))
  ;; the type specifier is (ARRAY ...)
  (unless (eq (first rest) '*)
    ;; The type specifier is either (ARRAY <type>) or (ARRAY
    ;; <type> ...).  In order for TYPEP to return
    ;; true, the element type of the array must be the
    ;; same as the result of upgrading <type>.  FIXME:
    ;; is EQUAL the right thing to do here?
    (unless (same-array-element-type-p (array-element-type object)
				       (upgraded-array-element-type
					(first rest)))
      ;; No luck, they are not the same.
      (return-from typep-compound nil)))
  ;; The element types are compatible.  Check whether
  ;; we have  (ARRAY <type>) or (ARRAY <type> ...).
  (when (null (rest rest))
    ;; We have (ARRAY <type>) so we are done.
    (return-from typep-compound t))
  ;; We have (ARRAY <type> ...).
  ;; Check whether we have (ARRAY <type> *)
  (when (eq (second rest) '*)
    ;; We are done.
    (return-from typep-compound t))
  ;; Check whether we have (ARRAY <type> rank), and if
  ;; so whether the rank of OBJECT corresponds.
  (when (integerp (second rest))
    (return-from typep-compound
      (= (array-rank object) (second rest))))
  ;; We must have (ARRAY <type> (...)).  Start by
  ;; checking that the rank of OBJECT is the same as
  ;; the length of the list.
  (unless (= (array-rank object) (length (second rest)))
    (return-from typep-compound nil))
  ;; The ranks are the same.  Now check that each
  ;; dimension is valid.
  (loop for d1 in (array-dimensions object)
	for d2 in (second rest)
	unless (or (eq d1 '*) (= d1 d2))
	  do (return-from typep-compound nil))
  ;; Every dimension is valid.
  (return-from typep-compound t))

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
