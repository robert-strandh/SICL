(in-package #:cleavir-ast)

;;;; This file contains definitions of AST classes that have to do
;;;; with fixnum arithmetic and comparison. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-add-AST.
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

(defclass fixnum-add-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(defun make-fixnum-add-ast (arg1-ast arg2-ast variable-ast)
  (make-instance 'fixnum-add-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast
    :variable-ast variable-ast))

(cleavir-io:define-save-info fixnum-add-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:variable-ast variable-ast))

(defmethod children ((ast fixnum-add-ast))
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

(defclass fixnum-subast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)
   (%variable-ast :initarg :variable-ast :reader variable-ast)))

(defun make-fixnum-subast (arg1-ast arg2-ast variable-ast)
  (make-instance 'fixnum-subast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast
    :variable-ast variable-ast))

(cleavir-io:define-save-info fixnum-subast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast)
  (:variable-ast variable-ast))

(defmethod children ((ast fixnum-subast))
  (list (arg1-ast ast) (arg2-ast ast) (variable-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-less-AST.
;;;
;;; This class can be used to implement a binary LESS-THAN function.
;;; It requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-less-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-less-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-less-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-less-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-less-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-not-greater-AST.
;;;
;;; This class can be used to implement a binary <= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-not-greater-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-not-greater-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-not-greater-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-not-greater-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-not-greater-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-greater-AST.
;;;
;;; This class can be used to implement a binary GREATER-THAN
;;; function.  It requires both its arguments to be of type FIXNUM.
;;; It can only occur as the TEST-AST of an IF-AST.  If this AST
;;; occurs in a position where a value is required, an error is
;;; signaled.

(defclass fixnum-greater-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-greater-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-greater-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-greater-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-greater-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-not-less-AST.
;;;
;;; This class can be used to implement a binary >= function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-not-less-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-not-less-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-not-less-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-not-less-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-not-less-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FIXNUM-equal-AST.
;;;
;;; This class can be used to implement a binary = function.  It
;;; requires both its arguments to be of type FIXNUM.  It can only
;;; occur as the TEST-AST of an IF-AST.  If this AST occurs in a
;;; position where a value is required, an error is signaled.

(defclass fixnum-equal-ast (ast)
  ((%arg1-ast :initarg :arg1-ast :reader arg1-ast)
   (%arg2-ast :initarg :arg2-ast :reader arg2-ast)))

(defun make-fixnum-equal-ast (arg1-ast arg2-ast)
  (make-instance 'fixnum-equal-ast
    :arg1-ast arg1-ast
    :arg2-ast arg2-ast))

(cleavir-io:define-save-info fixnum-equal-ast
  (:arg1-ast arg1-ast)
  (:arg2-ast arg2-ast))

(defmethod children ((ast fixnum-equal-ast))
  (list (arg1-ast ast) (arg2-ast ast)))

