(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ARRAY-ACCESS-AST
;;;
;;; Abstract parent for ASTs accessing array elements.
;;; ARRAY-AST will evaluate to the array, and INDEX-AST
;;; will evaluate to the row-major-index of the element.
;;; ELEMENT-TYPE is the actual element-type of the array.
;;; SIMPLE-P is whether it's actually simple. BOXED-P is whether
;;; the values in the array are boxed or not, which
;;; is used during AST-to-HIR to insert un/box instructions.

(defclass array-access-ast (ast)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%element-type :initarg :element-type :reader element-type)
   (%simple-p :initarg :simple-p :reader simple-p)
   (%boxed-p :initarg :boxed-p :reader boxed-p)))

(cleavir-io:define-save-info array-access-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:element-type element-type)
  (:simple-p simple-p)
  (:boxed-p boxed-p))

(defmethod map-children progn (function (ast array-access-ast))
  (funcall function (array-ast ast))
  (funcall function (index-ast ast)))
(defmethod children append ((ast array-access-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AREF-AST.
;;;
;;; This AST represents all cases of reading an element from an
;;; array where enough information about the array is known.
;;; If BOXED-P is false, an additional BOX-INSTRUCTION will
;;; be added at the output.

(defclass aref-ast (read-ast-mixin array-access-ast) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ASET-AST.
;;;
;;; This AST represents all cases of writing an element to an array
;;; where enough information about the array is known.
;;; The ELEMENT-AST evaluates to the element to be written, and that
;;; element must be of a type that is acceptable to store in the array,
;;; according to how the array is specialized.

(defclass aset-ast (write-ast-mixin array-access-ast)
  ())
