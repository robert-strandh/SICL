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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CHAR-CODE-AST.
;;;
;;; This AST can be used to convert a character to its code.

(defclass char-code-ast (ast one-value-ast-mixin side-effect-free-ast-mixin)
  ((%char-ast :initarg :char-ast :reader char-ast)))

(cleavir-io:define-save-info char-code-ast
  (:char-ast char-ast))

(defmethod children ((ast char-code-ast))
  (list (char-ast ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CODE-CHAR-AST.
;;;
;;; This AST can be used to convert a character code to a character.

(defclass code-char-ast (ast one-value-ast-mixin side-effect-free-ast-mixin)
  ((%code-ast :initarg :code-ast :reader code-ast)))

(cleavir-io:define-save-info code-char-ast
  (:code-ast code-ast))

(defmethod children ((ast code-char-ast))
  (list (code-ast ast)))
