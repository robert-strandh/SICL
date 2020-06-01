(cl:in-package #:cleavir-ast)

;;;; This file contains definitions of AST classes that have to do
;;;; with fixnum arithmetic and comparison. 

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
;;; this AST generates a FIXNUM that it writes to LVAR.  When
;;; there is no overflow, LVAR is the sum of the two FIXNUM
;;; arguments.  When there is an overflow, if the result is negative,
;;; then a BIGNUM with the value 2^n + LVAR should be created,
;;; where n is the number of bits in a word.  If LVAR is
;;; non-negative, then a BIGNUM with the value LVAR - 2^n should
;;; be created.

(defclass fixnum-add-ast (boolean-ast-mixin ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%lvar :initarg :lvar :reader lvar)))

(defun make-fixnum-add-ast (arg1-ast arg2-ast lvar &key origin (policy *policy*))
  (make-instance 'fixnum-add-ast
    :origin origin :policy policy
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast
    :lvar lvar))

(cleavir-io:define-save-info fixnum-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:lvar lvar))

(defmethod map-children progn (function (ast fixnum-add-ast))
  (funcall function (arg1-ast ast))
  (funcall function (arg2-ast ast)))
(defmethod children append ((ast fixnum-add-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

(defmethod map-variables progn (function (ast fixnum-add-ast))
  (funcall function (lvar ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-SUB-AST.
;;;
;;; This AST can be used to implement a binary subtraction function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the test of an IF-AST.  As a result of the operation,
;;; this AST generates a FIXNUM that it writes to LVAR.  When
;;; there is no overflow, LVAR is the sum of the two FIXNUM
;;; arguments.  When there is an overflow, if the result is negative,
;;; then a BIGNUM with the value 2^n + LVAR should be created,
;;; where n is the number of bits in a word.  If LVAR is
;;; non-negative, then a BIGNUM with the value LVAR - 2^n should
;;; be created.

(defclass fixnum-sub-ast (boolean-ast-mixin ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%lvar :initarg :lvar :reader lvar)))

(defun make-fixnum-sub-ast (arg1-ast arg2-ast lvar &key origin (policy *policy*))
  (make-instance 'fixnum-sub-ast
    :origin origin :policy policy
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast
    :lvar lvar))

(cleavir-io:define-save-info fixnum-sub-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:lvar lvar))

(defmethod map-children progn (function (ast fixnum-sub-ast))
  (funcall function (arg1-ast ast))
  (funcall function (arg2-ast ast)))
(defmethod children append ((ast fixnum-sub-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

(defmethod map-variables progn (function (ast fixnum-sub-ast))
  (funcall function (lvar ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for fixnum comparison.

(defmacro define-fixnum-comparison-ast (name)
  `(progn 
     (defclass ,name (boolean-ast-mixin ast)
       ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
	(%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

     (cleavir-io:define-save-info ,name
       (:arg1-ast arg1-ast)
       (:arg2-ast arg2-ast))

     (defmethod map-children progn (function (ast ,name))
       (funcall function (arg1-ast ast))
       (funcall function (arg2-ast ast)))
     (defmethod children append ((ast ,name))
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

