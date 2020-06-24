(in-package #:cleavir-ctype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions SUBTYPEP, UPGRADED-ARRAY-ELEMENT-TYPE,
;;; and UPGRADED-COMPLEX-PART-TYPE.
;;;
;;; As in CL, but with ctypes, no environment, and a client parameter. SUBTYPEP
;;; may only be called with two non-values ctypes or two values ctypes. U-A-E-T
;;; and U-C-P-T may not be called with values ctypes.

(defgeneric subtypep (ctype1 ctype2 system))

(defgeneric upgraded-array-element-type (ctype system))
(defgeneric upgraded-complex-part-type (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions TOP, BOTTOM.
;;;
;;; Return top or bottom ctypes (i.e. ctypes of T and NIL respectively).
;;; Required to do even very basic type operations.

(defgeneric top (system))
(defgeneric bottom (system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions TOP-P, BOTTOM-P.
;;;
;;; Return whether the given ctype is the top or bottom ctype respectively.
;;; These functions are intended to be quick rather than necessarily correct
;;; (which is uncomputable in the presence of SATISFIES anyway). If they return
;;; true, that must be accurate, but they are permitted to return false even if
;;; the ctype actually is top or bottom respectively.
;;;
;;; Neither function may be passed a values ctype.

(defgeneric top-p (ctype system))
(defgeneric bottom-p (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions CONJOIN/2, DISJOIN/2.
;;;
;;; Given two ctypes, compute their conjunction or disjunction, as for the AND
;;; and OR type specifiers respectively. The arguments may either both be values
;;; ctypes or both non-values ctypes, i.e. they are never called with a values
;;; and non-values ctype.
;;;
;;; Called by the n-ary CONJOIN and DISJOIN.

(defgeneric conjoin/2 (ctype1 ctype2 system))
(defgeneric disjoin/2 (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function NEGATE.
;;;
;;; Compute the negation of the given ctype, as if using the NOT type specifier.
;;; May not be called with a values ctype.

(defgeneric negate (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SUBTRACT.
;;;
;;; Compute the difference of the two ctypes.
;;; (subtract c1 c2 s) = (conjoin/2 c1 (negate c2 s) s), but this can sometimes
;;; be done more efficiently. May not be called with values ctypes.

(defgeneric subtract (ctype1 ctype2 system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONS.
;;;
;;; Given two non-values ctypes, return the ctype of a cons type using them.

(defgeneric cons (car cdr system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ARRAY.
;;;
;;; Given an element type specifier, dimensions specifier, and simplicity mark,
;;; return a ctype representing the array type.
;;; The element type specifier is either an upgraded non-values ctype,
;;; or the symbol *.
;;; The dimensions specifier is as in the CL type specifier ARRAY.
;;; The simplicity mark is one of the symbols ARRAY or SIMPLE-ARRAY.

(defgeneric array (element dimensions simplicity system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPLEX.
;;;
;;; Given a part type specifier, return a ctype for the complex type.
;;; The specifier may be either an upgraded non-values ctype or the symbol *.

(defgeneric complex (part system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function RANGE.
;;;
;;; Given a type name and interval designators, return a ctype.
;;; The type name is one of the symbols INTEGER, RATIONAL, REAL, FLOAT,
;;; SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, or LONG-FLOAT.

(defgeneric range (type low high system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MEMBER.

(defgeneric member (system &rest elements))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SATISFIES.

(defgeneric satisfies (fname system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION.
;;;
;;; Return a ctype for a function type specifier.
;;;
;;; The first seven parameters represent the parsed lambda list.
;;; REQUIRED and OPTIONAL are lists of non-values ctypes, and REST is a
;;; non-values ctype. KEYP and ALLOW-OTHER-KEYS-P indicate the presences of
;;; &key and &allow-other-keys respectively. KEYS is a list of
;;; (keyword non-values-ctype) elements.
;;; RETURNS is the values ctype of the return values specified.
;;;
;;; Note that REST must always be provided. If the function does not actually
;;; accept &rest arguments, this should be indicated by REST being the bottom
;;; ctype.

(defgeneric function
    (required optional rest keyp keys allow-other-keys-p returns system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VALUES.
;;;
;;; Return a ctype for a values type specifier.
;;;
;;; The first four parameters represent the parsed lambda list.
;;; REQUIRED and OPTIONAL are lists of ctypes. REST is a ctype.
;;;
;;; Note that REST must always be provided. To match the semantics of CL:THE, a
;;; values type specifier with no &rest may be considered to have an implicit
;;; &rest T, and that T ctype is expected to be provided by whatever code calls
;;; this function.
;;;
;;; The semantics of values types in the standard are self contradictory. For
;;; this system, the strict semantics described in the page on VALUES are
;;; ignored as being impractical. The semantics for CL:THE are used instead.
;;; So for example, (values 0 '(x)), (values 4), (values 23 nil 'values) are
;;; all valid forms in (the (values integer list) form), but (values) wouldn't
;;; be. This values type would be passed to this function with a REST that is
;;; the ctype for T.
;;;
;;; Values ctypes are only used in certain contexts, as described for the
;;; operators above.

(defgeneric values (required optional rest system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COERCE-TO-VALUES.
;;;
;;; Given a ctype, return a values ctype. In more detail, if the ctype is a
;;; values ctype, it is returned; otherwise, a values ctype for an equivalent to
;;; (values type &rest t) is returned, thus incorporating the fuzziness.

(defgeneric coerce-to-values (ctype system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic functions REQUIRED, OPTIONAL, REST, KEYSP, KEYS, ALLOW-OTHER-KEYS-P,
;;; and RETURNS.
;;;
;;; These are readers for function and values ctypes. Only the first three work
;;; with values ctypes; the rest are exclusive to function ctypes.

(defgeneric required (ctype system))
(defgeneric optional (ctype system))
(defgeneric rest (ctype system))
(defgeneric keysp (ctype system))
(defgeneric keys (ctype system))
(defgeneric allow-other-keys-p (ctype system))
(defgeneric returns (ctype system))
