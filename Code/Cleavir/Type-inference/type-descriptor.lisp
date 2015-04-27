(cl:in-package #:cleavir-type-inference)

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
;;;; A type descriptor is either T, meaning the type T, or it is a
;;;; list of sub-type descriptors.  The meaning of this list is the OR
;;;; of the meaning of each element.  If the list is empty, i.e. NIL,
;;;; it means the type NIL.  If a sub-type is omitted from the list,
;;;; then objects described by that sub-type are not members of the
;;;; type described by the type descriptor.
;;;;
;;;; Sub-type descriptors that are present in the type descriptor
;;;; appear in a fixed, predefined order.  This order is defined by
;;;; the order in which sub-type descriptors are defined below.
;;;;
;;;; INTEGER
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (INTEGER (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;;   where each li and ui is an integer or the symbol *.  Only l1
;;;;   and un can be the symbol *.  The pair (li ui) designates a
;;;;   closed interval of integers, where li is the least possible
;;;;   value and ui is the largest possible value.  Furthermore for
;;;;   all i, if both li and ui are integers, then i <= 1 <= n, li <=
;;;;   ui.  Also, for all i such that 1 <= i < n, ui + 1 < li+1.  In
;;;;   other words, the intervals are ordered and there is at least
;;;;   one integer between each interval.  Notice that we do not allow
;;;;   the notation (n) for the sub-type INTEGER because it can
;;;;   trivially be replaced by n-1 or n+1 as needed.
;;;;
;;;; RATIONAL
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (RATIONAL (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;;   where each li and ui is either of the form r, (r), or the
;;;;   symbol *, where r is a rational number.  Only l1 and un can be
;;;;   the symbol *.  The pair (li ui) designates an interval of
;;;;   rationals.

;;  LocalWords:  canonicalize inferencer
