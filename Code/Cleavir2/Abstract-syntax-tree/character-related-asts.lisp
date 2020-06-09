(cl:in-package #:cleavir-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHARACTERP-AST.
;;;
;;; This AST can be used by implementations that represent characters as
;;; immediate objects with tags.  It can only occur as the test of an
;;; IF-AST.

(defclass characterp-ast (ast boolean-ast-mixin)
  ((%object-ast :initarg :object-ast :reader object-ast)))

(cleavir-io:define-save-info characterp-ast
  (:object-ast object-ast))

(defmethod children ((ast characterp-ast))
  (list (object-ast ast)))
