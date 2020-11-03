(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    (client (ast ast:aref-ast) lexical-environment)
  `(row-major-aref 
    ,(translate-ast client (ast:array-ast ast) lexical-environment)
    ,(translate-ast client (ast:index-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:aset-ast) lexical-environment)
  `(setf (row-major-aref 
          ,(translate-ast client (ast:array-ast ast) lexical-environment)
          ,(translate-ast client (ast:index-ast ast) lexical-environment))
         ,(translate-ast client (ast:element-ast ast) lexical-environment)))
