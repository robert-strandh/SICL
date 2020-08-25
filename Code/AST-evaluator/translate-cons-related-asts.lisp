(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:consp-ast) global-environment lexical-environment)
  `(car
    ,(translate-ast (ast:object-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:car-ast) global-environment lexical-environment)
  `(car
    ,(translate-ast (ast:cons-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:cdr-ast) global-environment lexical-environment)
  `(cdr
    ,(translate-ast (ast:cons-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:rplaca-ast) global-environment lexical-environment)
  `(rplaca
    ,(translate-ast (ast:cons-ast ast)  global-environment lexical-environment)
    ,(translate-ast (ast:object-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:rplacd-ast) global-environment lexical-environment)
  `(rplacd
    ,(translate-ast (ast:cons-ast ast)  global-environment lexical-environment)
    ,(translate-ast (ast:object-ast ast)  global-environment lexical-environment)))
