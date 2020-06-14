(cl:in-package #:cleavir-ast)

;;;; This file contains definitions of AST classes that have to do
;;;; with fixnum arithmetic and comparison. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUMP-AST.
;;;
;;; This AST can be used by implementations that represent fixnums as
;;; immediate objects with tags.  It can only occur as the test of an
;;; IF-AST.

(defclass fixnump-ast (ast boolean-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)))

(cleavir-io:define-save-info fixnump-ast
  (:object-ast object-ast))

(defmethod children ((ast fixnump-ast))
  (list (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for fixnum arithmetic.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-ADD-AST.
;;;
;;; This AST can be used to implement a binary addition function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the test of an IF-AST.  As a result of the operation,
;;; this AST generates a FIXNUM that it writes to VARIABLE.  When
;;; there is no overflow, VARIABLE is the sum of the two FIXNUM
;;; arguments.  When there is an overflow, if the result is negative,
;;; then a BIGNUM with the value 2^n + VARIABLE should be created,
;;; where n is the number of bits in a word.  If VARIABLE is
;;; non-negative, then a BIGNUM with the value VARIABLE - 2^n should
;;; be created.

(defclass fixnum-add-ast (ast boolean-ast-mixin)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(cleavir-io:define-save-info fixnum-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:variable-ast variable-ast))

(defmethod children ((ast fixnum-add-ast))
  (list (arg1-ast ast) (arg2-ast ast) (variable-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-SUB-AST.
;;;
;;; This AST can be used to implement a binary subtraction function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the test of an IF-AST.  As a result of the operation,
;;; this AST generates a FIXNUM that it writes to VARIABLE.  When
;;; there is no overflow, VARIABLE is the sum of the two FIXNUM
;;; arguments.  When there is an overflow, if the result is negative,
;;; then a BIGNUM with the value 2^n + VARIABLE should be created,
;;; where n is the number of bits in a word.  If VARIABLE is
;;; non-negative, then a BIGNUM with the value VARIABLE - 2^n should
;;; be created.

(defclass fixnum-sub-ast (ast boolean-ast-mixin)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(cleavir-io:define-save-info fixnum-sub-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:variable-ast variable-ast))

(defmethod children ((ast fixnum-sub-ast))
  (list (arg1-ast ast) (arg2-ast ast) (variable-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-DIVIDE-AST.
;;;
;;; This AST can be used to implement a binary division function.  It
;;; requires its first argument to evaluate to a non-negative FIXNUM
;;; and its second argument to evaluate to a positive fixnum.  This
;;; AST occur only as the FORM-AST of a MULTIPLE-VALUE-SETQ-AST with
;;; exactly two ASTs in the list LHS-ASTS.  As a result of the
;;; operation, the first left-hand side of the MULTIPLE-VALUE-SETQ-AST
;;; will contain the quotient between the two arguments, and the
;;; second left-hand side of the MULTIPLE-VALUE-SETQ-AST will contain
;;; the remainder.  Rounding is towards zero as with the Common Lisp
;;; function FLOOR.

(defclass fixnum-divide-ast (ast boolean-ast-mixin)
  ((%dividend-ast :initarg :dividend-ast :reader dividend-ast)
   (%divisor-ast :initarg :divisor-ast :reader divisor-ast)))

(cleavir-io:define-save-info fixnum-divide-ast
  (:dividend-ast dividend-ast)
  (:divisor-ast divisor-ast))

(defmethod children ((ast fixnum-divide-ast))
  (list (dividend-ast ast) (divisor-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for fixnum comparison.

(defmacro define-fixnum-comparison-ast (name)
  `(progn 
     (defclass ,name (ast boolean-ast-mixin)
       ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
        (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

     (cleavir-io:define-save-info ,name
       (:arg1-ast arg1-ast)
       (:arg2-ast arg2-ast))

     (defmethod children ((ast ,name))
       (list (arg1-ast ast) (arg2-ast ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-LESS-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-fixnum-comparison-ast fixnum-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-NOT-GREATER-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-fixnum-comparison-ast fixnum-not-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-GREATER-AST.
;;;
;;; This class can be used to implement a binary GREATER-THAN
;;; function.  It requires both its arguments to be of type FIXNUM.
;;; It can only occur as the TEST-AST of an IF-AST.  If this AST
;;; occurs in a position where a value is required, an error is
;;; signaled.

(define-fixnum-comparison-ast fixnum-greater-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-NOT-LESS-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-fixnum-comparison-ast fixnum-not-less-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-EQUAL-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(define-fixnum-comparison-ast fixnum-equal-ast)
