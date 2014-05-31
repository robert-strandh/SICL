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
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(defun make-fixnum-+-ast (arg1-ast arg2-ast variable-ast)
  (make-instance 'fixnum-+-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast
    :variable-ast variable-ast))

(cleavir-io:define-save-info fixnum-+-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:variable-ast variable-ast))

(defmethod children ((ast fixnum-+-ast))
  (list (arg1-ast ast) (arg2-ast ast) (variable-ast ast)))

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
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(defun make-fixnum---ast (arg1-ast arg2-ast variable-ast)
  (make-instance 'fixnum---ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast
    :variable-ast variable-ast))

(cleavir-io:define-save-info fixnum---ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:variable-ast variable-ast))

(defmethod children ((ast fixnum---ast))
  (list (arg1-ast ast) (arg2-ast ast) (variable-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-<-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-<-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-<-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-<-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-<-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-<-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-<=-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-<=-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-<=-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-<=-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-<=-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-<=-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

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
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum->-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum->-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum->-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum->-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM->=-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum->=-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum->=-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum->=-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum->=-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum->=-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-=-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-=-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-=-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-=-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-=-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-=-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

