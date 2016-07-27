(cl:in-package #:cleavir-ast)

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
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-simple-t-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'simple-t-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info simple-t-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast simple-t-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INTRICATE-T-AREF-AST.
;;;
;;; This AST can be used to read an element of a non-simple
;;; unspecialized array.  It corresponds roughly to the standard
;;; function ROW-MAJOR-AREF.

(defclass intricate-t-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-intricate-t-aref-ast (array-ast index-ast &key origin)
  (make-instance 'intricate-t-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info intricate-t-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast intricate-t-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INTRICATE-T-ASET-AST
;;;
;;; This AST can be used to write an element of a non-simple
;;; unspecialized array.  It corresponds roughly to a function (SETF
;;; ROW-MAJOR-ASET).  An attempt to compile this AST in a context
;;; where a value is needed will result in an error being signaled.

(defclass intricate-t-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-intricate-t-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'intricate-t-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info intricate-t-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast intricate-t-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of an array specialized to
;;; SHORT-FLOAT.

(defclass short-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-short-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'short-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info short-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast short-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SHORT-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of an array specialized
;;; to SHORT-FLOAT.

(defclass short-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-short-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'short-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info short-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast short-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of an array specialized to
;;; SINGLE-FLOAT.

(defclass single-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-single-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'single-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info single-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast single-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SINGLE-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of an array specialized
;;; to SINGLE-FLOAT.

(defclass single-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-single-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'single-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info single-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast single-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of an array specialized to
;;; DOUBLE-FLOAT.

(defclass double-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-double-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'double-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info double-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast double-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DOUBLE-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of an array specialized
;;; to DOUBLE-FLOAT.

(defclass double-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-double-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'double-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info double-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast double-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-AREF-AST
;;;
;;; This AST can be used to read an element of an array specialized to
;;; LONG-FLOAT.

(defclass long-float-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-long-float-aref-ast (array-ast index-ast &key origin)
  (make-instance 'long-float-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info long-float-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast long-float-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LONG-FLOAT-ASET-AST
;;;
;;; This AST can be used to write an element of an array specialized
;;; to LONG-FLOAT.

(defclass long-float-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-long-float-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'long-float-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info long-float-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast long-float-aset-ast))
  (list (array-ast ast) (index-ast ast) (value-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIT-AREF-AST
;;;
;;; This AST can be used to read an element of an array specialized to
;;; BIT.

(defclass bit-aref-ast (ast one-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)))

(defun make-bit-aref-ast (array-ast index-ast &key origin)
  (make-instance 'bit-aref-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast))

(cleavir-io:define-save-info bit-aref-ast
  (:array-ast array-ast)
  (:index-ast index-ast))

(defmethod children ((ast bit-aref-ast))
  (list (array-ast ast) (index-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BIT-ASET-AST
;;;
;;; This AST can be used to write an element of an array specialized
;;; to BIT.

(defclass bit-aset-ast (ast no-value-ast-mixin)
  ((%array-ast :initarg :array-ast :reader array-ast)
   (%index-ast :initarg :index-ast :reader index-ast)
   (%value-ast :initarg :value-ast :reader value-ast)))

(defun make-bit-aset-ast (array-ast index-ast value-ast &key origin)
  (make-instance 'bit-aset-ast
    :origin origin
    :array-ast array-ast
    :index-ast index-ast
    :value-ast value-ast))

(cleavir-io:define-save-info bit-aset-ast
  (:array-ast array-ast)
  (:index-ast index-ast)
  (:value-ast value-ast))

(defmethod children ((ast bit-aset-ast))
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
