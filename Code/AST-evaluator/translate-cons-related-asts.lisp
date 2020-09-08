(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:consp-ast) lexical-environment)
  `(car
    ,(translate-ast (ast:object-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:car-ast) lexical-environment)
  `(car
    ,(translate-ast (ast:cons-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:cdr-ast) lexical-environment)
  `(cdr
    ,(translate-ast (ast:cons-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:rplaca-ast) lexical-environment)
  `(rplaca
    ,(translate-ast (ast:cons-ast ast) lexical-environment)
    ,(translate-ast (ast:object-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:rplacd-ast) lexical-environment)
  `(rplacd
    ,(translate-ast (ast:cons-ast ast) lexical-environment)
    ,(translate-ast (ast:object-ast ast) lexical-environment)))
