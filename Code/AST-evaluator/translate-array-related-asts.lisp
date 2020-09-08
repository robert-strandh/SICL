(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:aref-ast) lexical-environment)
  `(row-major-aref 
    ,(translate-ast (ast:array-ast ast) lexical-environment)
    ,(translate-ast (ast:index-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:aset-ast) lexical-environment)
  `(setf (row-major-aref 
          ,(translate-ast (ast:array-ast ast) lexical-environment)
          ,(translate-ast (ast:index-ast ast) lexical-environment))
         ,(translate-ast (ast:element-ast ast) lexical-environment)))
