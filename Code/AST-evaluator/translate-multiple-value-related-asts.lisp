(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:multiple-value-setq-ast) lexical-environment)
  `(multiple-value-setq
       ,(loop for lhs-ast in (ast:lhs-asts ast)
              collect 
              (translate-ast lhs-ast lexical-environment))
     ,(translate-ast (ast:form-ast ast) lexical-environment)))

(defmethod translate-ast
    ((ast ast:multiple-value-call-ast) lexical-environment)
  `(multiple-value-call
       ,(translate-ast (ast:function-form-ast ast) lexical-environment)
     ,@(loop for form-ast in (ast:form-asts ast)
             collect 
             (translate-ast form-ast lexical-environment))))

(defmethod translate-ast
    ((ast ast:values-ast) lexical-environment)
  `(values
    ,@(loop for argument-ast in (ast:argument-asts ast)
            collect 
            (translate-ast argument-ast lexical-environment))))

(defmethod translate-ast
    ((ast ast:multiple-value-prog1-ast) lexical-environment)
  `(multiple-value-prog1
       ,(translate-ast (ast:first-form-ast ast) lexical-environment)
     ,@(loop for form-ast in (ast:form-asts ast)
             collect 
             (translate-ast form-ast lexical-environment))))
