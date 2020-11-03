(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    (client (ast ast:characterp-ast) lexical-environment)
  `(characterp
    ,(translate-ast
      client (ast:object-ast ast) lexical-environment)))

;;; FIXME: we might want to have these functions query the
;;; environment, rather then relying on the host having the same
;;; relationship between characters and character codes.  On the other
;;; hand, these functions should not be called at all during
;;; bootstrapping.

(defmethod translate-ast
    (client (ast ast:char-code-ast) lexical-environment)
  `(char-code
    ,(translate-ast
      client (ast:char-ast ast) lexical-environment)))
  
(defmethod translate-ast
    (client (ast ast:code-char-ast) lexical-environment)
  `(code-char
    ,(translate-ast
      client (ast:code-ast ast) lexical-environment)))
