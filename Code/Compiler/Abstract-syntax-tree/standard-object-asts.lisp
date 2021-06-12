(cl:in-package #:sicl-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RACK-AST.
;;;
;;; This AST has a single child which must return a standard object.
;;; The value of the AST is the rack of that standard object.

(defclass rack-ast (cleavir-ast:ast cleavir-ast:one-value-ast-mixin)
  ((%standard-object-ast
    :initarg :standard-object-ast
    :reader standard-object-ast)))

(cleavir-io:define-save-info rack-ast
  (:standard-object-ast standard-object-ast))

(defmethod cleavir-ast:children ((ast rack-ast))
  (list (standard-object-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SET-RACK-AST.
;;;
;;; This AST has two children.  The first child must return a standard
;;; object.  The second child must return a rack.  The effect of the
;;; AST is to set the rack of the standard object that is the value of
;;; the first child to the rack that is the value of the second child.
;;; This AST does not generate any value.  An attempt to compile this
;;; AST in a context where a value is needed will result in an error
;;; being signaled.

(defclass set-rack-ast (cleavir-ast:ast cleavir-ast:no-value-ast-mixin)
  ((%standard-object-ast
    :initarg :standard-object-ast
    :reader standard-object-ast)
   (%rack-ast :initarg :rack-ast :reader rack-ast)))

(cleavir-io:define-save-info set-rack-ast
  (:standard-object-ast standard-object-ast)
  (:rack-ast rack-ast))

(defmethod cleavir-ast:children ((ast set-rack-ast))
  (list (standard-object-ast ast) (rack-ast ast)))
