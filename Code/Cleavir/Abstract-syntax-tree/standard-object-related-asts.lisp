(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SLOT-ACCESS-AST.
;;;
;;; Abstract parent class for ASTs representing access to an instance's
;;; local slots. It has two children, an AST that must have a standard
;;; instance as its value, and an AST that must have a fixnum as its
;;; value and that indicates a slot number (starting from 0).

(defclass slot-access-ast (ast)
  ((%object-ast :initarg :object-ast :reader object-ast)
   (%slot-number-ast :initarg :slot-number-ast :reader slot-number-ast)))

(cleavir-io:define-save-info slot-access-ast
  (:object-ast object-ast)
  (:slot-number-ast slot-number-ast))

(defmethod map-children progn (function (ast slot-access-ast))
  (funcall function (object-ast ast))
  (funcall function (slot-number-ast ast)))
(defmethod children append ((ast slot-access-ast))
  (list (object-ast ast) (slot-number-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SLOT-READ-AST.
;;;
;;; This AST can be used to read a slot from a standard instance.
;;; This AST generates a single value, namely the contents of the slot
;;; with the number given.

(defclass slot-read-ast (read-ast-mixin slot-access-ast) ())

(defun make-slot-read-ast (object-ast slot-number-ast &key origin (policy *policy*))
  (make-instance 'slot-read-ast
    :origin origin :policy policy
    :object-ast object-ast
    :slot-number-ast slot-number-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SLOT-WRITE-AST.
;;;
;;; This AST can be used to write a slot in a standard instance.
;;; This AST generates no values.

(defclass slot-write-ast (write-ast-mixin slot-access-ast) ())

(defun make-slot-write-ast (object-ast slot-number-ast value-ast &key origin (policy *policy*))
  (make-instance 'slot-write-ast
    :origin origin :policy policy
    :object-ast object-ast
    :slot-number-ast slot-number-ast
    :value-ast value-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCALLABLE-SLOT-ACCESS-AST.
;;;
;;; Abstract parent class for ASTs representing access to a funcallable
;;; instance's local slots. It has two children, an AST that must have a
;;; funcallable instance as its value, and an AST that must have a fixnum
;;; as its value and that indicates a slot number (starting from 0).

(defclass funcallable-slot-access-ast (ast)
  ((%object-ast :initarg :object-ast :reader object-ast)
   (%slot-number-ast :initarg :slot-number-ast :reader slot-number-ast)))

(cleavir-io:define-save-info funcallable-slot-access-ast
  (:object-ast object-ast)
  (:slot-number-ast slot-number-ast))

(defmethod map-children progn (function (ast funcallable-slot-access-ast))
  (funcall function (object-ast ast))
  (funcall function (slot-number-ast ast)))
(defmethod children append ((ast funcallable-slot-access-ast))
  (list (object-ast ast) (slot-number-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCALLABLE-SLOT-READ-AST.
;;;
;;; This AST can be used to read a slot from a funcallable instance.  It
;;; has two children, an AST that must have a funcallable instance as its
;;; value, and an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0).  This AST generates a
;;; single value, namely the contents of the slot with the number given.

(defclass funcallable-slot-read-ast
    (read-ast-mixin funcallable-slot-access-ast)
  ())

(defun make-funcallable-slot-read-ast
    (object-ast slot-number-ast &key origin (policy *policy*))
  (make-instance 'funcallable-slot-read-ast
    :origin origin :policy policy
    :object-ast object-ast
    :slot-number-ast slot-number-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCALLABLE-SLOT-WRITE-AST.
;;;
;;; This AST can be used to write a slot in a funcallable instance.  It
;;; has three children, an AST that must have a funcallable instance as
;;; its value, an AST that must have a fixnum as its value and that
;;; indicates a slot number (starting from 0), and an AST that
;;; generates the new value to store in the slot.  This AST generates
;;; no values.  An attempt to compile this AST in a context where a
;;; value is needed will result in an error being signaled.

(defclass funcallable-slot-write-ast
    (write-ast-mixin funcallable-slot-access-ast)
  ())

(defun make-funcallable-slot-write-ast
    (object-ast slot-number-ast value-ast &key origin (policy *policy*))
  (make-instance 'funcallable-slot-write-ast
    :origin origin :policy policy
    :object-ast object-ast
    :slot-number-ast slot-number-ast
    :value-ast value-ast))
