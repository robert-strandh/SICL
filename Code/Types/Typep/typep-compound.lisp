(cl:in-package #:sicl-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Here is what we know about types.
;;;
;;; Standard types and user-defined types....
;;;
;;; Purpose: implement TYPEP...
;;;
;;; The types CONS, SYMBOL, ARRAY, NUMBER, CHARACTER, HASH-TABLE,
;;; FUNCTION, READTABLE, PACKAGE, PATHNAME, STREAM, RANDOM-STATE,
;;; CONDITION, and RESTART are pairwise disjoint.
;;;

;;; If the type specifier is a symbol for which there is deftype
;;; expander or a list whose CAR is such a symbol, we apply the
;;; expander and try again.
;;;
;;; If not, and if the type specifier is a class C or a symbol that is
;;; the name of a class C, we check whether the CLASS-OF the object is
;;; a subclass of the class C.
;;;
;;; If not, and if the type specifier is a symbol, then it must be one
;;; of the following symbols (which don't have any corresponding
;;; system class): atom, base-char, standard-char, extended-char,
;;; base-string, simple-string, simple-base-string, simple-bit-vector,
;;; bignum, bit, signed-byte, unsigned-byte, compiled-function,
;;; short-float, single-float, double-float, long-float, fixnum, nil,
;;; simple-array, simple-vector.  We can easily define deftype
;;; expanders for: atom, base-char, standard-char, extended-char,
;;; bignum, bit, signed-byte, unsigned-byte, base-string,
;;; simple-string, simple-base-string, simple-bit-vector,
;;; simple-vector.  Nothing is of type nil.  We are left with:
;;; compiled-function, short-float, single-float, double-float,
;;; long-float, simple-array.
;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Standardized atomic type specifiers.
;;;
;;; Many standardized atomic type specifiers have corresponding system
;;; classes.  For such a type s, we can easily check whether an object
;;; o is of that type by checking that c is in the class precede list
;;; of the class of o, where c is the system class corresponding to s.

;;; The following standardized atomic types are conditions, so they
;;; are subclasses of the class CONDITION.  None of them takes any
;;; arguments, so they can only be used as atomic type specifiers.
;;;
;;; arithmetic-error, simple-condition, simple-error,
;;; simple-type-error, simple-warning, cell-error, storage-condition,
;;; package-error, stream-error, parse-error, condition,
;;; print-not-readable, control-error, program-error,
;;; division-by-zero, style-warning, end-of-file, reader-error, error,
;;; type-error, file-error, unbound-slot, unbound-variable,
;;; serious-condition, undefined-function, floating-point-inexact,
;;; floating-point-invalid-operation, floating-point-overflow,
;;; floating-point-underflow, warning.

;;; The remaining standardized types that can be used only as atomic
;;; type specifiers are these:
;;;
;;; Character types: character, base-char, standard-char,
;;; extended-char.
;;;
;;; There is a built-in predicate CHARACTERP to test whether an object
;;; is a character.  The other character types can be defined using
;;; this predicate and SATISFIES with a functions that check the range
;;; of the character code or by using a bitmap.
;;;
;;; Stream types: stream, broadcast-stream, concatenated-stream,
;;; string-stream, echo-stream, synonym-stream, two-way-stream,
;;; file-stream.
;;;
;;; There is a built-in predicate STREAMP to test whether an object is
;;; a stream.  The other stream types have corresponding system
;;; classes (as does STREAM), that are all pairwise disjoint.  So if
;;; we do not allow subclasses, then TYPEP for streams can be
;;; implemented by checking the CLASS-OF the object for equality (EQ)
;;; with the FIND-CLASS of the name.
;;;
;;; Number types: number, ratio, bit, fixnum, bignum
;;;
;;; There are built-in predicates NUMBERP, INTEGERP, RATIONALP, REALP.
;;; A number is a ratio if it is rational and not integer.  The types
;;; bit, fixnum, and bignum can be defined in terms of ranges of
;;; integers. For fixnum and bignum, use MOST-POSITIVE-FIXNUM and
;;; MOST-NEGATIVE-FIXNUM.
;;;
;;; Standard object types: generic-function,
;;; standard-generic-function, class, standard-class, built-in-class,
;;; structure-class, standard-object, structure-object, method,
;;; standard-method, method-combination,
;;;
;;; Others: t, null, nil, atom, hash-table, keyword, logical-pathname,
;;; list, compiled-function, package, pathname, random-state, symbol,
;;; readtable, sequence, restart,
;;;
;;; Everything is of type t, nothing is of type nil.  Only NIL is of
;;; type NULL, so use EQ.  ATOM is the same as NOT CONS. There is a
;;; predicate KEYWORDP.  There is a predicate HASH-TABLE-P.  For
;;; pathnames, the situation is similar to that of streams.  LIST is
;;; CONS or NULL. There is a predicate COMPILED-FUNCTION-P.  There is
;;; a predicate PACKAGEP.  There is a predicate RANDOM-STATE-P. There
;;; is a predicate SYMBOLP. There is a predicate READTABLEP, SEQUENCE
;;; is LIST or VECTOR.  For RESTART we can use class-of.

;;; The following standardized types can only be used as compound
;;; type specifiers:
;;;
;;; and, eql, member, mod, not, or, satisfies, values.


;;; The following standardized types can be used both as atomic and as
;;; compound type specifiers:

;;; function, array, simple-string, integer, base-string,
;;; simple-vector, single-float, bit-vector, long-float, complex,
;;; string, cons, double-float, rational, real, float, short-float,
;;; unsigned-byte, signed-byte, vector, simple-array,
;;; simple-base-string, simple-bit-vector

;;; Implement TYPEP for a type specifier of the form (HEAD . REST).
;;; OBJECT is the object to test.  ENVIRONMENT is the environment to
;;; use for expanding types defined by DEFTYPE.
(defgeneric typep-compound (object head rest environment))

;;; Given a type specifier that is illegal in the context of TYPEP,
;;; such as VALUES or FUNCTION, signal an error to that effect.
(defmethod typep-compound (object head rest environment)
  (declare (ignore object environment))
  (error "Compound type specifier is illegal for typep: ~s."
	 (cons head rest)))

;;; Given a type specifier of the form (AND . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'and)) rest environment)
  (loop for type-spec in rest
	always (generic-typep object type-spec environment)))

;;; Given a type specifier of the form (EQL . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'eql)) rest environment)
  (declare (ignore environment))
  (eql object (first rest)))

;;; Given a type specifier of the form (MEMBER . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'member)) rest environment)
  (declare (ignore environment))
  (member object rest))

;;; Given a type specifier of the form (NOT . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'not)) rest environment)
  (not (generic-typep object (first rest) environment)))

;;; Given a type specifier of the form (OR . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'or)) rest environment)
  (loop for type-spec in rest
	when (generic-typep object type-spec environment)
	  return t))

;;; Given a type specifier of the form (SATISFIES . REST), check
;;; whether OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'satisfies)) rest environment)
  (funcall (sicl-genv:fdefinition (first rest) environment)
	   object))

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

;;; Given a type specifier of the form (ARRAY . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'array)) rest environment)
  (declare (ignore environment))
  (unless (arrayp object)
    (return-from typep-compound nil))
  ;; OBJECT is definitely an array.
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

;;; Given a type specifier of the form (COMPLEX . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'complex)) rest environment)
  (unless (complexp object)
    (return-from typep-compound nil))
  ;; OBJECT is definitely a complex.
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
	(and (generic-typep (realpart object) type environment)
	     (generic-typep (imagpart object) type environment)))))

;;; Given a type specifier of the form (CONS . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'cons)) rest environment)
  (unless (consp object)
    (return-from typep-compound nil))
  ;; OBJECT is definitely a CONS.
  (when (null rest)
    ;; the type specifier is (CONS), so since OBJECT is a CONS, we
    ;; are done.
    (return-from typep-compound t))
  ;; the type specifier is (CONS <type> . ...)
  (unless (generic-typep (car object) (first rest) environment)
    (return-from typep-compound nil))
  ;; The CAR of OBJECT is the right type.  Now check the CDR.
  (when (null (rest rest))
    ;; the type specifier is (CONS <type>), so since OBJECT is a CONS, and
    ;; the CAR is the right type, we are done.
    (return-from typep-compound t))
  ;; the type specifier is (CONS <type> <type>)
  (generic-typep (cdr object) (second rest) environment))

;;; Given a type specifier of the form (INTEGER . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'integer)) rest environment)
  (unless (integerp object)
    (return-from typep-compound nil))
  ;; OBJECT is definitely an integer.
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

;;; Given a type specifier of the form (RATIONAL . REST), check whether
;;; OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'rational)) rest environment)
  (unless (rationalp object)
    (return-from typep-compound nil))
  ;; OBJECT is definitely an rational.
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
  
;;; Given a type specifier of the form (SHORT-FLOAT . REST), check
;;; whether OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'short-float)) rest environment)
  (declare (ignore environment))
  (typep-compound-float object head rest))

;;; Given a type specifier of the form (SINGLE-FLOAT . REST), check
;;; whether OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'single-float)) rest environment)
  (declare (ignore environment))
  (typep-compound-float object head rest))

;;; Given a type specifier of the form (DOUBLE-FLOAT . REST), check
;;; whether OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'double-float)) rest environment)
  (declare (ignore environment))
  (typep-compound-float object head rest))

;;; Given a type specifier of the form (LONG-FLOAT . REST), check
;;; whether OBJECT is of that type in ENVIRONMENT.
(defmethod typep-compound (object (head (eql 'long-float)) rest environment)
  (declare (ignore environment))
  (typep-compound-float object head rest))
