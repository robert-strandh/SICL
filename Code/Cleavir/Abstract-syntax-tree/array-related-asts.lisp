(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AREF-AST.
;;;
;;; This AST represents all cases of reading an element from an
;;; array where enough information about the array is known. The
;;; INDEX-AST is an AST that evaluates to a row-major index into
;;; the array that is the value of ARRAY-AST. ELEMENT-TYPE is the
;;; actual element-type of the array. SIMPLE-P is whether it's
;;; actually simple. BOXED-P is whether the values in the array
;;; are boxed or not (if not, an additional BOX-INSTRUCTION will
;;; be added at the output)

(defclass aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%element-type :initarg :element-type :reader element-type)
   (%simple-p :initarg :simple-p :reader simple-p)
   (%boxed-p :initarg :boxed-p :reader boxed-p)))

(cleavir-io:define-save-info aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:element-type element-type)
  (:simple-p simple-p)
  (:boxed-p boxed-p))

(defmethod children ((ast aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ASET-AST.
;;;
;;; This AST represents all cases of writing an element to an array
;;; where enough information about the array is known. The INDEX-AST
;;; is an AST that evaluates to a row-major index into the array that
;;; is the value of ARRAY-AST.  The ELEMENT-AST evaluates to the
;;; element to be written, and that element must be of a type that is
;;; acceptable to store in the array, according to how the array is
;;; specialized. SIMPLE-P is whether the array is actually simple and
;;; ELEMENT-TYPE is its actual element-type. BOXED-P indicates whether
;;; the values in the array are boxed or not (if not an additional
;;; UNBOX-INSTRUCTION will be added at the input)

(defclass aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%element-ast :initarg :element-ast :reader element-ast)
   (%element-type :initarg :element-type :reader element-type)
   (%simple-p :initarg :simple-p :reader simple-p)
   (%boxed-p :initarg :boxed-p :reader boxed-p)))

(cleavir-io:define-save-info aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:element-ast element-ast)
  (:element-type element-type)
  (:simple-p simple-p)
  (:boxed-p boxed-p))

(defmethod children ((ast aset-ast))
  (list (array-ast ast) (index-ast ast) (element-ast ast)))
