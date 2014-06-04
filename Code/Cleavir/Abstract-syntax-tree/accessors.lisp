(in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAR-AST.
;;;
;;; This AST can be used to implement the function CAR.  However, it
;;; does not correspond exactly to the function CAR, because the value
;;; of the single child must be a CONS cell. 

(defclass car-ast (ast one-value-ast-mixin)
  ((%cons-ast :initarg :cons-ast :reader cons-ast)))

(defun make-car-ast (cons-ast)
  (make-instance 'car-ast
    :cons-ast cons-ast))

(cleavir-io:define-save-info car-ast
  (:cons-ast cons-ast))

(defmethod children ((ast car-ast))
  (list (cons-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CDR-AST.
;;;
;;; This AST can be used to implement the function CDR.  However, it
;;; does not correspond exactly to the function CDR, because the value
;;; of the single child must be a CONS cell. 

(defclass cdr-ast (ast one-value-ast-mixin)
  ((%cons-ast :initarg :cons-ast :reader cons-ast)))

(defun make-cdr-ast (cons-ast)
  (make-instance 'cdr-ast
    :cons-ast cons-ast))

(cleavir-io:define-save-info cdr-ast
  (:cons-ast cons-ast))

(defmethod children ((ast cdr-ast))
  (list (cons-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RPLACA-AST.
;;;
;;; This AST can be used to implement the function RPLACA and the
;;; function (SETF CAR) in implementations where it is a function.
;;; This AST differs from the function RPLACA in that it does not
;;; generate any value.  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass rplaca-ast (ast no-value-ast-mixin)
  ((%cons-ast :initarg :cons-ast :reader cons-ast)
   (%object-ast :initarg :object-ast :reader object-ast)))

(defun make-rplaca-ast (cons-ast object-ast)
  (make-instance 'rplaca-ast
    :cons-ast cons-ast
    :object-ast object-ast))

(cleavir-io:define-save-info rplaca-ast
  (:cons-ast cons-ast)
  (:object-ast object-ast))

(defmethod children ((ast rplaca-ast))
  (list (cons-ast ast) (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RPLACD-AST.
;;;
;;; This AST can be used to implement the function RPLACD and the
;;; function (SETF CDR) in implementations where it is a function.
;;; This AST differs from the function RPLACD in that it does not
;;; generate any value.  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass rplacd-ast (ast no-value-ast-mixin)
  ((%cons-ast :initarg :cons-ast :reader cons-ast)
   (%object-ast :initarg :object-ast :reader object-ast)))

(defun make-rplacd-ast (cons-ast object-ast)
  (make-instance 'rplacd-ast
    :cons-ast cons-ast
    :object-ast object-ast))

(cleavir-io:define-save-info rplacd-ast
  (:cons-ast cons-ast)
  (:object-ast object-ast))

(defmethod children ((ast rplacd-ast))
  (list (cons-ast ast) (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SLOT-READ-AST.
;;;
;;; This AST can be used to read a slot from a standard instance.  It
;;; has two children, an AST that must have a standard instance as its
;;; value, and an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0).  This AST generates a
;;; single value, namely the contents of the slot with the number given.

(defclass slot-read-ast (ast one-value-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)
   (%slot-number-ast :initarg :slot-number-ast :reader slot-number-ast)))

(defun make-slot-read-ast (object-ast slot-number-ast)
  (make-instance 'slot-read-ast
    :object-ast object-ast
    :slot-number-ast slot-number-ast))

(cleavir-io:define-save-info slot-read-ast
  (:object-ast object-ast)
  (:slot-number-ast slot-number-ast))

(defmethod children ((ast slot-read-ast))
  (list (object-ast ast) (slot-number-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SLOT-WRITE-AST.
;;;
;;; This AST can be used to write a slot in a standard instance.  It
;;; has three children, an AST that must have a standard instance as
;;; its value, an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0), and an AST that
;;; generates the new value to store in the slot.  This AST generates
;;; no values.  An attempt to compile this AST in a context where a
;;; value is needed will result in an error being signaled.

(defclass slot-write-ast (ast no-value-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)
   (%slot-number-ast :initarg :slot-number-ast :reader slot-number-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-slot-write-ast (object-ast slot-number-ast value-ast)
  (make-instance 'slot-write-ast
    :object-ast object-ast
    :slot-number-ast slot-number-ast
    :value-ast value-ast))

(cleavir-io:define-save-info slot-write-ast
  (:object-ast object-ast)
  (:slot-number-ast slot-number-ast)
  (:value-ast value-ast))

(defmethod children ((ast slot-write-ast))
  (list (object-ast ast) (slot-number-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AREF-AST
;;;
;;; This AST can be used to read an element of an unspecialized array.
;;; It corresponds roughly to the standard function ROW-MAJOR-AREF.

(defclass aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-aref-ast (array-ast index-ast)
  (make-instance 'aref-ast
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ASET-AST
;;;
;;; This AST can be used to write an element of an unspecialized
;;; array.  It corresponds roughly to a function (SETF
;;; ROW-MAJOR-ASET).  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-aset-ast (array-ast index-ast value-ast)
  (make-instance 'aset-ast
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))
