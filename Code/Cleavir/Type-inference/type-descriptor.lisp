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
;;;; sub-classes of STANDARD-OBJECT.
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
;;;;     type CONS.
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
;;;;   (SIMPLE-ARRAY BIT)
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of BIT.
;;;;
;;;;   (SIMPLE-ARRAY UNSIGNED-BYTE 8)
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     8).
;;;;
;;;;   (SIMPLE-ARRAY UNSIGNED-BYTE 16)
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     16).
;;;;
;;;;   (SIMPLE-ARRAY UNSIGNED-BYTE 32)
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     32).
;;;;
;;;;   (SIMPLE-ARRAY UNSIGNED-BYTE 64)
;;;;
;;;;     This type descriptor means that the variable must contain a
;;;;     simple array with an upgraded element type of (UNSIGNED-BYTE
;;;;     64).
;;;;
;;;; COMPLEX
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (COMPLEX t1 t2 ... tn)
;;;;
;;;;   where t1, t2, ... tn are symbols taken from the set INTEGER,
;;;;   RATIONAL, SHORT-LOAT, SINGLE-FLOAT, DOUBLE-FLOAT, and
;;;;   LONG-FLOAT.  Each symbol can appear only once, and there must
;;;;   be at least one element in the list, or else the COMPLEX
;;;;   sub-type descriptor will be removed from the type descriptor.
;;;;
;;;;   Notice that we do not take into account the element type of the
;;;;   CONS.  We also do not canonicalize the occurrence of both NULL
;;;;   and CONS to LIST.

;;  LocalWords:  canonicalize inferencer
