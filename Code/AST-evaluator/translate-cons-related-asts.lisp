(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    (client (ast ast:consp-ast) lexical-environment)
  `(car
    ,(translate-ast client (ast:object-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:car-ast) lexical-environment)
  `(car
    ,(translate-ast client (ast:cons-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:cdr-ast) lexical-environment)
  `(cdr
    ,(translate-ast client (ast:cons-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:rplaca-ast) lexical-environment)
  `(rplaca
    ,(translate-ast client (ast:cons-ast ast) lexical-environment)
    ,(translate-ast client (ast:object-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:rplacd-ast) lexical-environment)
  `(rplacd
    ,(translate-ast client (ast:cons-ast ast) lexical-environment)
    ,(translate-ast client (ast:object-ast ast) lexical-environment)))
