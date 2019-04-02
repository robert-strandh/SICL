(cl:in-package #:cleavir-lexical)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPE-EXPAND.
;;;
;;; This function performs type macroexpansion (type macros being
;;; defined by DEFTYPE) in the given environment.  It expands only
;;; top-level type specifiers, i.e., it does not traverse nested type
;;; specifiers that are arguments to a top-level type specifier such
;;; as AND or ARRAY (for the element type).  However, it repeatedly
;;; expands the top-level type specifier until a built-in type
;;; specifier is obtained.  In this respect TYPE-EXPAND is like
;;; MACROEXPAND and unlike MACROEXPAND-1 or MACROEXPAND-ALL.

(defgeneric type-expand (client environment type-specifier))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function HAS-EXTENDED-CHAR-P.
;;;
;;; This function returns a Boolean value, indicating whether the
;;; implementation has a non-empty EXTENDED-CHAR type.  Recall that an
;;; implementation may decide to have all characters be BASE-CHARs, in
;;; which case this function returns NIL.  This function is called
;;; during type inference.

(defgeneric has-extended-char-p (client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FLOAT-TYPES.
;;;
;;; This function returns a list of floating point types available in
;;; the implementation.  The Common Lisp standard requires the
;;; implementation to support only one of a limited number of
;;; combinations, namely (SINGLE-FLOAT), (SHORT-FLOAT SINGLE-FLOAT),
;;; (SINGLE-FLOAT DOUBLE-FLOAT), (SHORT-FLOAT SINGLE-FLOAT
;;; DOUBLE-FLOAT), (SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT), or
;;; (SHORT-FLOAT SINGLE-FLOAT DOUBLE-FLOAT LONG-FLOAT).  This function
;;; is called during type inference.

(defgeneric float-types (client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPGRADED-COMPLEX-PART-TYPES.
;;;
;;; Returns a list of element types for distinct complex types in
;;; the implementation, e.g. (SINGLE-FLOAT DOUBLE-FLOAT REAL)
;;; Used during type inference.

(defgeneric upgraded-complex-part-types (client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function UPGRADED-ARRAY-ELEMENT-TYPES.
;;;
;;; Returns a list of element types for distinct array types in the
;;; implementation, e.g. the minimum is (BIT BASE-CHAR CHARACTER T)
;;; Used during type inference.

(defgeneric upgraded-array-element-types (client))
