(in-package #:cleavir-ast)

;;;; This file contains definitions of AST classes that have to do
;;;; with fixnum arithmetic and comparison. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-ARITHMETIC-AST.
;;;
;;; This AST can be thought of as a combination of an assignment (like
;;; SETQ) and a test (like IF).  It has four children.  The first
;;; child is a LEXICAL-AST.  The second child is an AST especially
;;; designed for this context that both produces a value in the
;;; LEXICAL-AST and evaluates one of the remaining children according
;;; to weather the operation generated an overflow. 

(defclass fixnum-arithmetic-ast (ast)
  ())

(defun make-fixnum-arithmetic-ast
    (variable-ast operation-ast normal-ast overflow-ast)
  (make-instance 'fixnum-arithmetic-ast
    :children (list variable-ast operation-ast normal-ast overflow-ast)))

(defmethod variable-ast ((ast fixnum-arithmetic-ast))
  (first (children ast)))

(defmethod operation-ast ((ast fixnum-arithmetic-ast))
  (second (children ast)))

(defmethod normal-ast ((ast fixnum-arithmetic-ast))
  (third (children ast)))

(defmethod overflow-ast ((ast fixnum-arithmetic-ast))
  (fourth (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-+-AST.
;;;
;;; This AST can be used to implement a binary addition function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the OPERATION-AST of a FIXNUM-ARITHMETIC-AST.  As a
;;; result of the operation, this AST generates a FIXNUM that it
;;; writes to VARIABLE.  When there is no overflow, VARIABLE is the
;;; sum of the two FIXNUM arguments.  When there is an overflow, if
;;; the result is negative, then a BIGNUM with the value 2^n +
;;; VARIABLE should be created, where n is the number of bits in a
;;; word.  If VARIABLE is non-negative, then a BIGNUM with the value
;;; VARIABLE - 2^n should be created.

(defclass fixnum-+-ast (ast)
  ())

(defun make-fixnum-+-ast (arg1-ast arg2-ast variable-ast)
  (make-instance 'fixnum-+-ast
    :children (list arg1-ast arg2-ast variable-ast)))

(defmethod arg1-ast ((ast fixnum-+-ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum-+-ast))
  (second (children ast)))

(defmethod variable-ast ((ast fixnum-+-ast))
  (third (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM---AST.
;;;
;;; This AST can be used to implement a binary subtraction function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the OPERATION-AST of a FIXNUM-ARITHMETIC-AST.  As a
;;; result of the operation, this AST generates a FIXNUM that it
;;; writes to VARIABLE.  When there is no overflow, VARIABLE is the
;;; sum of the two FIXNUM arguments.  When there is an overflow, if
;;; the result is negative, then a BIGNUM with the value 2^n +
;;; VARIABLE should be created, where n is the number of bits in a
;;; word.  If VARIABLE is non-negative, then a BIGNUM with the value
;;; VARIABLE - 2^n should be created.

(defclass fixnum---ast (ast)
  ())

(defun make-fixnum---ast (arg1-ast arg2-ast variable-ast)
  (make-instance 'fixnum---ast
    :children (list arg1-ast arg2-ast variable-ast)))

(defmethod arg1-ast ((ast fixnum---ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum---ast))
  (second (children ast)))

(defmethod variable-ast ((ast fixnum---ast))
  (third (children ast)))
