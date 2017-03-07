(cl:in-package #:cleavir-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPE-EXPAND.
;;;
;;; Performs type macroexpansion (macros being defined by DEFTYPE)
;;; in the given environment. Only top-level, but should expand
;;; repeatedly. That is to say, this is macroexpand, not
;;; macroexpand-1 or macroexpand-all.

(defgeneric type-expand (environment type-specifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function HAS-EXTENDED-CHAR-P.
;;;
;;; Returns a boolean indicating whether the implementation has a
;;; non-empty EXTENDED-CHAR type. (An implementation may decide to
;;; have all characters be BASE-CHARs, in which case this returns
;;; NIL.)
;;; Used during type inference.

(defgeneric has-extended-char-p (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FLOAT-TYPES.
;;;
;;; Returns a list of floating point types available in the
;;; implementation, that is, a list with SHORT-FLOAT, SINGLE-FLOAT,
;;; DOUBLE-FLOAT, and LONG-FLOAT zero or one times each. If two of
;;; the types are the same, only one should be returned.
;;; For example, in an implementation where (subtypep short single)
;;; this might return (SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT).
;;; Used during type inference.

(defgeneric float-types (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPGRADED-COMPLEX-PART-TYPES.
;;;
;;; Returns a list of element types for distinct complex types in
;;; the implementation, e.g. (SINGLE-FLOAT DOUBLE-FLOAT REAL)
;;; Used during type inference.

(defgeneric upgraded-complex-part-types (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPGRADED-ARRAY-ELEMENT-TYPES.
;;;
;;; Returns a list of element types for distinct array types in the
;;; implementation, e.g. the minimum is (BIT BASE-CHAR CHARACTER T)
;;; Used during type inference.

(defgeneric upgraded-array-element-types (environment))
