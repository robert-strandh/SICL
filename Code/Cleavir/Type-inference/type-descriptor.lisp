(cl:in-package #:cleavir-type-inference)

;;;; The only goal of (at least this version of) the type inferencer
;;;; is to speed up program execution by avoiding type tests and
;;;; box/unbox operations whenever possible.  It is not a goal to
;;;; report type violations to the user, though we might add that at
;;;; some later time.  This goal has some important consequences that
;;;; allow us to simplify the type inferencer considerably.
;;;;
;;;; For example, knowing that a variable contains an object of type
;;;; INTEGER, or NUMBER, or FLOAT is not useful, because in order to
;;;; apply some operation to it, some tests must be executed in order
;;;; to determine which sub-type of that type it is.  Similarly, it is
;;;; not useful to know that the type is either SINGLE-FLOAT or
;;;; DOUBLE-FLOAT because a test is required to determine which one it
;;;; is.
;;;;
;;;; We do not think it is useful to keep information about the
;;;; dimensions of an array.  We think that array operations need to
;;;; be optimized when they are in a loop, and then we count on
;;;; loop-invariant optimizations to factor out such computations.  We
;;;; do need to keep information about the upgraded element type of
;;;; the array, however, because it is essential in order to avoid
;;;; box/unbox operations on the elements.  We do not think it is
;;;; useful to know that some variable is an array, without also
;;;; knowing its exact upgraded element type, because a run-time test
;;;; would have to be made to determine this information.  For that
;;;; reason, we canonicalize to T when we do not know the exact
;;;; upgraded element type.

;;;; We use a canonical representation of the type of a variable.
;;;;
;;;; Baker's paper on SUBTYPEP suggests that it is not necessary to
;;;; canonicalize the representation of the type, and it is probably
;;;; best not to do so when SUBTYPEP is called a single time at
;;;; run-time.  However, in the type inferencer, the requirements are
;;;; quite different.  We frequently need to compute the union,
;;;; intersection, and difference of types, as well as many
;;;; compositions of these operations.  In that situation it is
;;;; probably better to use a canonical representation to avoid that
;;;; the size of the representation grow after each operation.
;;;;
;;;; The canonical representation we use is not a valid Common Lisp
;;;; type specifier.  For that reason, we call it a TYPE DESCRIPTOR
;;;; instead.
;;;;
;;;; For the type inferencer, we need a finite lattice.  We should
;;;; make sure we do not attempt to represent too fine-grain types, or
;;;; types that are not likely to be useful in practice.  So for
;;;; instance, a type such as (OR FIXNUM ARRAY) is not useful in
;;;; practice, because in order to do something with an object of that
;;;; type, it would first have to be tested to determine whether it is
;;;; one or the other.  Then, we might as well use the type T instead.
;;;; Furthermore, types such as (OR T1 T2) where T1 and T2 have
;;;; totally different representations do not occur often in practice,
;;;; so we do not attempt to represent them.
;;;;
;;;; Another important consideration for the lattice used here is that
;;;; an instance of STANDARD-OBJECT can have its class changed at any
;;;; time.  Therefore, it is not meaningful to distinguish between
;;;; subclasses of STANDARD-OBJECT.  We go one step further and
;;;; consider that any operation on a STANDARD-OBJECT requires a test
;;;; to determine the class of the object.  For that reason, we
;;;; canonicalize any STANDARD-OBJECT to T.
;;;;
;;;; Similarly, operations on other objects such as conditions,
;;;; streams, packages, symbols, etc. are infrequent, so a type test
;;;; will not impact overall performance.  Therefore, these types are
;;;; also considered to be T.
;;;;
;;;; Valid type descriptors:
;;;;
;;;;   T
;;;;
;;;;     This type descriptor means that the variable can have a value
;;;;     of any type.
;;;;
;;;;   NIL
;;;;
;;;;     This type descriptor means that the variable can not have any
;;;;     value at all.  A branch in which at least one variable has
;;;;     this type can not be executed, so the entire branch can be
;;;;     eliminated.
;;;;
;;;;   NULL
;;;;
;;;;     This type descriptor means that the variable must contain the
;;;;     value NIL.
;;;;
;;;;   CONS
;;;;
;;;;     This type descriptor means that the variable must have the
;;;;     type CONS.  Notice that we do not take into account the
;;;;     element type of the CONS.  We also do not canonicalize the
;;;;     occurrence of both NULL and CONS to LIST.
;;;;
;;;;   FIXNUM
;;;;
;;;;     This type descriptor means that the variable must have the
;;;;     type FIXNUM.
;;;;
;;;;   SHORT-FLOAT
;;;;
;;;;     This type descriptor means that the variable must have the
;;;;     type SHORT-FLOAT.
;;;;
;;;;   SINGLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must have the
;;;;     type SINGLE-FLOAT.
;;;;
;;;;   DOUBLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must have the
;;;;     type DOUBLE-FLOAT.
;;;;
;;;;   LONG-FLOAT
;;;;
;;;;     This type descriptor means that the variable must have the
;;;;     type LONG-FLOAT.
;;;;
;;;;   UNBOXED-SHORT-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain an
;;;;     unboxed SHORT-FLOAT value.
;;;;
;;;;   UNBOXED-SINGLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain an
;;;;     unboxed SINGLE-FLOAT value.
;;;;
;;;;   UNBOXED-DOUBLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain an
;;;;     unboxed DOUBLE-FLOAT value.
;;;;
;;;;   UNBOXED-LONG-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain an
;;;;     unboxed LONG-FLOAT value.
;;;;
;;;;   COMPLEX-SHORT-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     complex number with an upgraded element type of SHORT-FLOAT.
;;;;
;;;;   COMPLEX-SINGLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     complex number with an upgraded element type of SINGLE-FLOAT.
;;;;
;;;;   COMPLEX-DOUBLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     complex number with an upgraded element type of DOUBLE-FLOAT.
;;;;
;;;;   COMPLEX-LONG-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     complex number with an upgraded element type of LONG-FLOAT.
;;;;
;;;;   SIMPLE-ARRAY-BIT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of BIT.
;;;;
;;;;   SIMPLE-ARRAY-UNSIGNED-BYTE-8
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     8).
;;;;
;;;;   SIMPLE-ARRAY-UNSIGNED-BYTE-16
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     16).
;;;;
;;;;   SIMPLE-ARRAY-UNSIGNED-BYTE-32
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     32).
;;;;
;;;;   SIMPLE-ARRAY-UNSIGNED-BYTE-64
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     64).
;;;;
;;;;   SIMPLE-ARRAY-SIGNED-BYTE-8
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (SIGNED-BYTE
;;;;     8).
;;;;
;;;;   SIMPLE-ARRAY-SIGNED-BYTE-16
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (SIGNED-BYTE
;;;;     16).
;;;;
;;;;   SIMPLE-ARRAY-SIGNED-BYTE-32
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (SIGNED-BYTE
;;;;     32).
;;;;
;;;;   SIMPLE-ARRAY-SIGNED-BYTE-64
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (SIGNED-BYTE
;;;;     64).
;;;;
;;;;   SIMPLE-ARRAY-BASE-CHAR
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of BASE-CHAR.
;;;;
;;;;   SIMPLE-ARRAY-CHARACTER
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of CHARACTER.
;;;;
;;;;   SIMPLE-ARRAY-SHORT-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of SHORT-FLOAT.
;;;;
;;;;   SIMPLE-ARRAY-SINGLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of SINGLE-FLOAT.
;;;;
;;;;   SIMPLE-ARRAY-DOUBLE-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of DOUBLE-FLOAT.
;;;;
;;;;   SIMPLE-ARRAY-LONG-FLOAT
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of LONG-FLOAT.

;;;; Ideally, other code in this system will not worry itself with
;;;;  subtypep and so on, and only use the functions here.
;;;; The basic type versus descriptor rules are as follows:
;;;; 1) TYPEQ, etc. store their original types, not descriptors.
;;;;    No reason to lose information.
;;;; 2) Dictionaries and bags only store descriptors.
;;;;    Call approximate- and canonicalize-type a lot.
;;;; 3) All functions in this file return descriptors, not counting
;;;;     type-equal, top-p, bottom-p.
;;;;    Other than type-equal, approximate- and canonicalize-type,
;;;;     they only accept descriptors.

;;;; An alternate implementation option for this code is to lean
;;;;  heavily on cl:subtypep: join = `(or ,d1 ,d2), so on. If this
;;;;  is done, a coarser lattice can still be used by only storing
;;;;  canonicalized-types in the dictionary. But it means unboxed
;;;;  types are annoying (you could use satisfies, maybe?)

;;; Map of CL types to type descriptors.
;;; Checked left to right, so put more specific types first.
;;; Could be fixed at compile time.
(defparameter *cl-type->descriptors*
  '((nil nil)
    (fixnum fixnum)
    (null null)
    (symbol symbol)
    (cons cons)
    (short-float short-float)
    (single-float single-float)
    (double-float double-float)
    (long-float long-float)
    ((simple-array bit) simple-array-bit)
    ((simple-array base-char) simple-array-base-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function APPROXIMATE-TYPE.
;;;
;;; Converts a CL type specifier into whatever descriptor is
;;;  smallest that still fits it. Useful for THE; see note below.
;;; FIXME: handle more types.
;;; FIXME: environment junk (typexpand).

(defun approximate-type (type)
  (loop for (ctype descriptor) in *cl-type->descriptors*
	when (subtypep type ctype)
	  do (return-from approximate-type descriptor))
  ;; default
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CANONICALIZE-TYPE.
;;;
;;; Converts a CL type specifier into an equivalent descriptor.
;;; If this is successful, returns (values descriptor t), otherwise
;;;  (values nil nil).
;;; This is necessary and distinct from approximate-type for the
;;;  sake of typeq. E.g., consider (typeq (eql 3)). It is correct
;;;  to use the descriptor fixnum and say that the "then" branch
;;;  has whatever variable as a fixnum; but incorrect to say that
;;;  in the "else" branch it is not a fixnum.

;; unexported helper
(defun type-equal (t1 t2)
  (and (subtypep t1 t2) (subtypep t2 t1)))

(defun canonicalize-type (type)
  (loop for (ctype descriptor) in *cl-type->descriptors*
	when (type-equal type ctype)
	  do (return-from canonicalize-type descriptor))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Descriptor operations: TOP-P, BOTTOM-P.
;;;
;;; True if a descriptor is the top (T) or bottom (NIL) type, resp.

(defun top-p (descriptor) (eq descriptor 't))
(defun bottom-p (descriptor) (eq descriptor 'nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lattice operations: Functions BINARY-JOIN, BINARY-MEET.

(defgeneric binary-join (descriptor1 descriptor2))

(defgeneric binary-meet (descriptor1 descriptor2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function DIFFERENCE.
;;;
;;; (difference a b) = (meet a (negate b))
;;; but negate is difficult, and we only actually need difference
;;;  for typeq.

(defgeneric difference (descriptor1 descriptor2))

;; we only actually need more methods if we include descriptors
;;  that are subtypes of one another.
(defmethod difference (descriptor1 descriptor2)
  (if (eq descriptor1 descriptor2)
      nil ; x - x = 0
      descriptor1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BINARY-JOIN.

(defmethod binary-join (descriptor1 descriptor2)
  (if (eq descriptor1 descriptor2)
      descriptor1
      t))

(defmethod binary-join ((descriptor1 (eql 'nil)) descriptor2)
  descriptor2)
(defmethod binary-join (descriptor1 (descriptor2 (eql 'nil)))
  descriptor1)

(defmethod binary-join ((descriptor1 (eql 't)) descriptor2) 't)
(defmethod binary-join (descriptor1 (descriptor2 (eql 't))) 't)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on BINARY-MEET.

(defmethod binary-meet (descriptor1 descriptor2)
  (declare (ignore descriptor2))
  ;; we could just as well return descriptor2. the true result is
  ;;  a subtype of both of them.
  descriptor1)

(defmethod binary-meet ((descriptor1 (eql 'nil)) descriptor2) nil)
(defmethod binary-meet (descriptor1 (descriptor2 (eql 'nil))) nil)
(defmethod binary-meet ((descriptor1 (eql 't)) descriptor2)
  descriptor2)
(defmethod binary-meet (descriptor1 (descriptor2 (eql 't)))
  descriptor1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on both.
(macrolet ((disjoin (d1 d2)
	     `(progn
		(defmethod binary-join ((descriptor1 (eql ',d1))
					(descriptor2 (eql ',d2)))
		  t)
		(defmethod binary-join ((descriptor1 (eql ',d2))
					(descriptor2 (eql ',d1)))
		  t)
		(defmethod binary-meet ((descriptor1 (eql ',d1))
					(descriptor2 (eql ',d2)))
		  nil)
		(defmethod binary-meet ((descriptor1 (eql ',d2))
					(descriptor2 (eql ',d1)))
		  nil)))
	   (pairwise (&rest descriptors)
	     `(progn
		,@(loop for (d1 . rest) on descriptors
			append (loop for d2 in rest
				     collect `(disjoin ,d1 ,d2))))))
  (pairwise fixnum null cons
	    short-float single-float double-float long-float
	    simple-array-bit simple-array-base-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function JOIN.

(defun join (&rest descriptors)
  (reduce #'binary-join descriptors :initial-value 'nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MEET.

(defun meet (&rest descriptors)
  (reduce #'binary-meet descriptors :initial-value 't))

;;  LocalWords:  canonicalize inferencer
