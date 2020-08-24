(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:aref-ast) global-environment lexical-environment)
  `(row-major-aref 
    ,(translate-ast (ast:array-ast ast)  global-environment lexical-environment)
    ,(translate-ast (ast:index-ast ast)  global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:aset-ast) global-environment lexical-environment)
  `(setf (row-major-aref 
          ,(translate-ast (ast:array-ast ast)  global-environment lexical-environment)
          ,(translate-ast (ast:index-ast ast)  global-environment lexical-environment))
         ,(translate-ast (ast:element-ast ast)  global-environment lexical-environment)))
