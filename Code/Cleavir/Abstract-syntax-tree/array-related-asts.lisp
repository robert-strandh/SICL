(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AREF-AST.
;;;
;;; This AST is the base class for all AST classes used to read an
;;; element from an array, whether unspecialized or specialized.  The
;;; INDEX-AST is an AST that evaluates to a row-major index into the
;;; array that is the value of ARRAY-AST.

(defclass aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(cleavir-io:define-save-info aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ASET-AST.
;;;
;;; This AST is the base class for all AST classes used to write an
;;; element to an array, whether unspecialized or specialized.  The
;;; INDEX-AST is an AST that evaluates to a row-major index into the
;;; array that is the value of ARRAY-AST.  The ELEMENT-AST evaluates
;;; to the element to be written, and that element must be of a type
;;; that is acceptable to store in the array, according to how the
;;; array is specialized.

(defclass aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%element-ast :initarg :element-ast :reader element-ast)))

(cleavir-io:define-save-info aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:element-ast element-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-T-AREF-AST.
;;;
;;; This AST can be used to read an element of a simple unspecialized
;;; array.  It corresponds roughly to the standard function
;;; ROW-MAJOR-AREF.

(defclass simple-t-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-simple-t-aref-ast (array-ast index-ast &key origin)
  (make-instance 'simple-t-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info simple-t-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast simple-t-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-T-ASET-AST
;;;
;;; This AST can be used to write an element of a a simple
;;; unspecialized array.  It corresponds roughly to a function (SETF
;;; ROW-MAJOR-ASET).  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass simple-t-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%object-ast :initarg :object-ast :reader object-ast)))

(defun make-simple-t-aset-ast (array-ast index-ast object-ast &key origin)
  (make-instance 'simple-t-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :object-ast object-ast))

(cleavir-io:define-save-info simple-t-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:object-ast object-ast))

(defmethod children ((ast simple-t-aset-ast))
  (list (array-ast ast) (index-ast ast) (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-T-AREF-AST.
;;;
;;; This AST can be used to read an element of a non-simple
;;; unspecialized array.  It corresponds roughly to the standard
;;; function ROW-MAJOR-AREF.

(defclass non-simple-t-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-non-simple-t-aref-ast (array-ast index-ast &key origin)
  (make-instance 'non-simple-t-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info non-simple-t-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast non-simple-t-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-T-ASET-AST
;;;
;;; This AST can be used to write an element of a non-simple
;;; unspecialized array.  It corresponds roughly to a function (SETF
;;; ROW-MAJOR-ASET).  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass non-simple-t-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-non-simple-t-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'non-simple-t-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info non-simple-t-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast non-simple-t-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-SHORT-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a simple array
;;; specialized to SHORT-FLOAT.

(defclass simple-short-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-simple-short-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'simple-short-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info simple-short-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast simple-short-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-SHORT-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element to a simple array
;;; specialized to SHORT-FLOAT.

(defclass simple-short-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%object-ast :initarg :object-ast :reader object-ast)))

(defun make-simple-short-float-aset-ast (array-ast index-ast object-ast &key origin)
  (make-instance 'simple-short-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :object-ast object-ast))

(cleavir-io:define-save-info simple-short-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:object-ast object-ast))

(defmethod children ((ast simple-short-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-SHORT-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a non-simple array
;;; specialized to SHORT-FLOAT.

(defclass non-simple-short-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-non-simple-short-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'non-simple-short-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info non-simple-short-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast non-simple-short-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-SHORT-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element to a non-simple array
;;; specialized to SHORT-FLOAT.

(defclass non-simple-short-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%object-ast :initarg :object-ast :reader object-ast)))

(defun make-non-simple-short-float-aset-ast
    (array-ast index-ast object-ast &key origin)
  (make-instance 'non-simple-short-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :object-ast object-ast))

(cleavir-io:define-save-info non-simple-short-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:object-ast object-ast))

(defmethod children ((ast non-simple-short-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-SINGLE-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a simple array
;;; specialized to SINGLE-FLOAT.

(defclass simple-single-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-simple-single-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'simple-single-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info simple-single-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast simple-single-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-SINGLE-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of a simple array
;;; specialized to SINGLE-FLOAT.

(defclass simple-single-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-simple-single-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'simple-single-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info simple-single-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast simple-single-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-SINGLE-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a non-simple array
;;; specialized to SINGLE-FLOAT.

(defclass non-simple-single-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-non-simple-single-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'non-simple-single-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info non-simple-single-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast non-simple-single-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-SINGLE-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of a non-simple array
;;; specialized to SINGLE-FLOAT.

(defclass non-simple-single-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-non-simple-single-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'non-simple-single-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info non-simple-single-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast non-simple-single-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-DOUBLE-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a simple array
;;; specialized to DOUBLE-FLOAT.

(defclass simple-double-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-simple-double-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'simple-double-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info simple-double-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast simple-double-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-DOUBLE-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of a simple array
;;; specialized to DOUBLE-FLOAT.

(defclass simple-double-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-simple-double-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'simple-double-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info simple-double-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast simple-double-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-DOUBLE-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a non-simple array
;;; specialized to DOUBLE-FLOAT.

(defclass non-simple-double-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-non-simple-double-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'non-simple-double-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info non-simple-double-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast non-simple-double-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-DOUBLE-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of a non-simple array
;;; specialized to DOUBLE-FLOAT.

(defclass non-simple-double-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-non-simple-double-float-aset-ast
    (array-ast index-ast value-ast &key origin)
  (make-instance 'non-simple-double-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info non-simple-double-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast non-simple-double-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-LONG-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a simple array
;;; specialized to LONG-FLOAT.

(defclass simple-long-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-simple-long-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'simple-long-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info simple-long-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast simple-long-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-LONG-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of a simple array
;;; specialized to LONG-FLOAT.

(defclass simple-long-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-simple-long-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'simple-long-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info simple-long-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast simple-long-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-LONG-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of a non-simple array
;;; specialized to LONG-FLOAT.

(defclass non-simple-long-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-non-simple-long-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'non-simple-long-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info non-simple-long-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast non-simple-long-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-LONG-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of a non-simple array
;;; specialized to LONG-FLOAT.

(defclass non-simple-long-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-non-simple-long-float-aset-ast
    (array-ast index-ast value-ast &key origin)
  (make-instance 'non-simple-long-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info non-simple-long-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast non-simple-long-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-BIT-AREF-AST
;;;
;;; This AST can be used to read an element of a simple array
;;; specialized to BIT.

(defclass simple-bit-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-simple-bit-aref-ast (array-ast index-ast &key origin)
  (make-instance 'simple-bit-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info simple-bit-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast simple-bit-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SIMPLE-BIT-ASET-AST
;;;
;;; This AST can be used to write an element of a simple array
;;; specialized to BIT.

(defclass simple-bit-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-simple-bit-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'simple-bit-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info simple-bit-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast simple-bit-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-BIT-AREF-AST
;;;
;;; This AST can be used to read an element of a non-simple array
;;; specialized to BIT.

(defclass non-simple-bit-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-non-simple-bit-aref-ast (array-ast index-ast &key origin)
  (make-instance 'non-simple-bit-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info non-simple-bit-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast non-simple-bit-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NON-SIMPLE-BIT-ASET-AST
;;;
;;; This AST can be used to write an element of a non-simple array
;;; specialized to BIT.

(defclass non-simple-bit-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-non-simple-bit-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'non-simple-bit-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info non-simple-bit-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast non-simple-bit-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNSIGNED-BYTE-8-AREF-AST
;;;
;;; This AST can be used to read an element of an array specialized to
;;; (UNSIGNED-BYTE 8)

(defclass unsigned-byte-8-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-unsigned-byte-8-aref-ast (array-ast index-ast &key origin)
  (make-instance 'unsigned-byte-8-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info unsigned-byte-8-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast unsigned-byte-8-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class UNSIGNED-BYTE-8-ASET-AST
;;;
;;; This AST can be used to write an element of an array specialized
;;; to (UNSIGNED-BYTE 8).

(defclass unsigned-byte-8-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-unsigned-byte-8-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'unsigned-byte-8-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info unsigned-byte-8-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast unsigned-byte-8-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))
