(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSP-AST.
;;;
;;; This AST can be used by implementations that identify CONS cells
;;; by pointer tags.  It can only occur as the test of an IF-AST.

(defclass consp-ast (ast boolean-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)))

(cleavir-io:define-save-info consp-ast
  (:object-ast object-ast))

(defmethod children ((ast consp-ast))
  (list (object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CAR-AST.
;;;
;;; This AST can be used to implement the function CAR.  However, it
;;; does not correspond exactly to the function CAR, because the value
;;; of the single child must be a CONS cell. 

(defclass car-ast (ast one-value-ast-mixin)
  ((%cons-ast :initarg :cons-ast :reader cons-ast)))

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

(cleavir-io:define-save-info rplacd-ast
  (:cons-ast cons-ast)
  (:object-ast object-ast))

(defmethod children ((ast rplacd-ast))
  (list (cons-ast ast) (object-ast ast)))
