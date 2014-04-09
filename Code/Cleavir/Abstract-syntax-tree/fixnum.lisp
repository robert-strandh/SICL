(in-package #:cleavir-ast)

;;;; This file contains definitions of AST classes that have to do
;;;; with fixnum arithmetic and comparison. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-ADD-AST.
;;;
;;; This AST can be used to implement a binary addition function.  It
;;; requires both its arguments to be of type FIXNUM.  It generates
;;; two values that we may call RESULT (a FIXNUM) and OVERFLOWP (a
;;; Boolean).  OVERFLOWP indicates whether the operation resulted in
;;; an overflow.  When it is false, then there was no overflow, and
;;; RESULT is the sum of the two arguments.  When OVERFLOWP is true,
;;; the operation resulted in an overflow.  Then, if RESULT is
;;; negative, then a BIGNUM with the value 2^n + RESULT should be
;;; created, where n is the number of bits in a word.  If RESULT is
;;; non-negative, then a BIGNUM with the value RESULT - 2^n should be
;;; created.

(defclass fixnum-add-ast (ast)
  ())

(defun make-fixnum-add-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-add-ast
    :children (list arg1-ast arg2-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-SUB-AST.
;;;
;;; This AST can be used to implement a binary subtraction function.
;;; It requires both its arguments to be of type FIXNUM.  It generates
;;; two values that we may call RESULT (a FIXNUM) and OVERFLOWP (a
;;; Boolean).  OVERFLOWP indicates whether the operation resulted in
;;; an overflow.  When it is false, then there was no overflow, and
;;; RESULT is the sum of the two arguments.  When OVERFLOWP is true,
;;; the operation resulted in an overflow.  Then, if RESULT is
;;; negative, then a BIGNUM with the value 2^n + RESULT should be
;;; created, where n is the number of bits in a word.  If RESULT is
;;; non-negative, then a BIGNUM with the value RESULT - 2^n should be
;;; created.

(defclass fixnum-sub-ast (ast)
  ())

(defun make-fixnum-sub-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-sub-ast
    :children (list arg1-ast arg2-ast)))

