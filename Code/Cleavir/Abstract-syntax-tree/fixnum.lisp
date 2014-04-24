(in-package #:cleavir-ast)

;;;; This file contains definitions of AST classes that have to do
;;;; with fixnum arithmetic and comparison. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-+-AST.
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
;;; occur as the test of an IF-AST.  As a result of the operation,
;;; this AST generates a FIXNUM that it writes to VARIABLE.  When
;;; there is no overflow, VARIABLE is the sum of the two FIXNUM
;;; arguments.  When there is an overflow, if the result is negative,
;;; then a BIGNUM with the value 2^n + VARIABLE should be created,
;;; where n is the number of bits in a word.  If VARIABLE is
;;; non-negative, then a BIGNUM with the value VARIABLE - 2^n should
;;; be created.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-<-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-<-ast (ast)
  ())

(defun make-fixnum-<-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-<-ast
    :children (list arg1-ast arg2-ast)))

(defmethod arg1-ast ((ast fixnum-<-ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum-<-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-<=-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-<=-ast (ast)
  ())

(defun make-fixnum-<=-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-<=-ast
    :children (list arg1-ast arg2-ast)))

(defmethod arg1-ast ((ast fixnum-<=-ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum-<=-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM->-AST.
;;;
;;; This class can be used to implement a binary GREATER-THAN
;;; function.  It requires both its arguments to be of type FIXNUM.
;;; It can only occur as the TEST-AST of an IF-AST.  If this AST
;;; occurs in a position where a value is required, an error is
;;; signaled.

(defclass fixnum->-ast (ast)
  ())

(defun make-fixnum->-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum->-ast
    :children (list arg1-ast arg2-ast)))

(defmethod arg1-ast ((ast fixnum->-ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum->-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM->=-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum->=-ast (ast)
  ())

(defun make-fixnum->=-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum->=-ast
    :children (list arg1-ast arg2-ast)))

(defmethod arg1-ast ((ast fixnum->=-ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum->=-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-=-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-=-ast (ast)
  ())

(defun make-fixnum-=-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-=-ast
    :children (list arg1-ast arg2-ast)))

(defmethod arg1-ast ((ast fixnum-=-ast))
  (first (children ast)))

(defmethod arg2-ast ((ast fixnum-=-ast))
  (second (children ast)))
