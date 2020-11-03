(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    (client (ast ast:multiple-value-setq-ast) lexical-environment)
  `(multiple-value-setq
       ,(loop for lhs-ast in (ast:lhs-asts ast)
              collect 
              (translate-ast client lhs-ast lexical-environment))
     ,(translate-ast client (ast:form-ast ast) lexical-environment)))

(defmethod translate-ast
    (client (ast ast:multiple-value-call-ast) lexical-environment)
  `(multiple-value-call
       ,(translate-ast client (ast:function-form-ast ast) lexical-environment)
     ,@(loop for form-ast in (ast:form-asts ast)
             collect 
             (translate-ast client form-ast lexical-environment))))

(defmethod translate-ast
    (client (ast ast:values-ast) lexical-environment)
  `(values
    ,@(loop for argument-ast in (ast:argument-asts ast)
            collect 
            (translate-ast client argument-ast lexical-environment))))

(defmethod translate-ast
    (client (ast ast:multiple-value-prog1-ast) lexical-environment)
  `(multiple-value-prog1
       ,(translate-ast client (ast:first-form-ast ast) lexical-environment)
     ,@(loop for form-ast in (ast:form-asts ast)
             collect 
             (translate-ast client form-ast lexical-environment))))
