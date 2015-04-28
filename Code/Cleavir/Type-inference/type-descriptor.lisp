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
;;;; For a sub-type descriptor that designates a set of INTERVALS, the
;;;; following restriction holds: Two intervals can not overlap or
;;;; touch, and the intervals are ordered from smallest to greatest.
;;;; For the RATIONAL type descriptor interval, the notation (x) means
;;;; that x is not part of the interval, so that the interval is open
;;;; in the direction indicated.  Other interval descriptors do not
;;;; allow the notation (x).  The first element of the first interval
;;;; may be the symbol * which means that that there is no lower
;;;; bound.  Similarly, the second element of the last interval may be
;;;; the symbol *, meaning that there is no upper bound.
;;;;
;;;; Notice that there is no FLOAT sub-type descriptor.  The
;;;; implementation has to supply a list of the XXX-FLOAT types that
;;;; it supports, so that an occurrence of a type-specifier involving
;;;; FLOAT will be turned into a type descriptor in which FLOAT has
;;;; been replaced by every supported XXX-FLOAT type.
;;;;
;;;; INTEGER
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (INTEGER (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;; RATIONAL
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (RATIONAL (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;; SHORT-FLOAT
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (SHORT-FLOAT (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;; SINGLE-FLOAT
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (SINGLE-FLOAT (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;; DOUBLE-FLOAT
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (DOUBLE-FLOAT (l1 u1) (l2 u2) ... (ln un))
;;;;
;;;; LONG-FLOAT
;;;;
;;;;   This sub-type descriptor has the following form:
;;;;
;;;;     (LONG-FLOAT (l1 u1) (l2 u2) ... (ln un))
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

;;  LocalWords:  canonicalize inferencer
