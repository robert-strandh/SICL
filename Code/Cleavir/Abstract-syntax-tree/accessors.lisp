(in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAR-AST.
;;;
;;; This AST can be used to implement the function CAR.  However, it
;;; does not correspond exactly to the function CAR, because the value
;;; of the single child must be a CONS cell. 

(defclass car-ast (ast)
  ())

(defun make-car-ast (cons-ast)
  (make-instance 'car-ast
    :children (list cons-ast)))

(defmethod cons-ast ((ast car-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CDR-AST.
;;;
;;; This AST can be used to implement the function CDR.  However, it
;;; does not correspond exactly to the function CDR, because the value
;;; of the single child must be a CONS cell. 

(defclass cdr-ast (ast)
  ())

(defun make-cdr-ast (cons-ast)
  (make-instance 'cdr-ast
    :children (list cons-ast)))

(defmethod cons-ast ((ast cdr-ast))
  (first (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RPLACA-AST.
;;;
;;; This AST can be used to implement the function RPLACA and the
;;; function (SETF CAR) in implementations where it is a function.
;;; This AST differs from the function RPLACA in that it does not
;;; generate any value.  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass rplaca-ast (ast)
  ())

(defun make-rplaca-ast (cons-ast object-ast)
  (make-instance 'rplaca-ast
    :children (list cons-ast object-ast)))

(defmethod cons-ast ((ast rplaca-ast))
  (first (children ast)))

(defmethod object-ast ((ast rplaca-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RPLACD-AST.
;;;
;;; This AST can be used to implement the function RPLACD and the
;;; function (SETF CDR) in implementations where it is a function.
;;; This AST differs from the function RPLACD in that it does not
;;; generate any value.  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass rplacd-ast (ast)
  ())

(defun make-rplacd-ast (cons-ast object-ast)
  (make-instance 'rplacd-ast
    :children (list cons-ast object-ast)))

(defmethod cons-ast ((ast rplacd-ast))
  (first (children ast)))

(defmethod object-ast ((ast rplacd-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SI-READ-AST.
;;;
;;; This AST can be used to read a slot from a standard instance.  It
;;; has two children, an AST that must have a standard instance as its
;;; value, and an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0).  This AST generates a
;;; single value, namely the contents of the slot with the number given.

(defclass si-read-ast (ast)
  ())

(defun make-si-read-ast (si-ast slot-number-ast)
  (make-instance 'si-read-ast
    :children (list si-ast slot-number-ast)))

(defmethod si-ast ((ast si-read-ast))
  (first (children ast)))

(defmethod slot-number-ast ((ast si-read-ast))
  (second (children ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SI-WRITE-AST.
;;;
;;; This AST can be used to write a slot in a standard instance.  It
;;; has three children, an AST that must have a standard instance as
;;; its value, an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0), and an AST that
;;; generates the new value to store in the slot.  This AST generates
;;; no values. 

(defclass si-write-ast (ast)
  ())

(defun make-si-write-ast (si-ast slot-number-ast value-ast)
  (make-instance 'si-write-ast
    :children (list si-ast slot-number-ast value-ast)))

(defmethod si-ast ((ast si-write-ast))
  (first (children ast)))

(defmethod slot-number-ast ((ast si-write-ast))
  (second (children ast)))

(defmethod value-ast ((ast si-write-ast))
  (third (children ast)))
