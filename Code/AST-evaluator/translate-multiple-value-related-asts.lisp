(cl:in-package #:sicl-ast-evaluator)

(defmethod translate-ast
    ((ast ast:multiple-value-setq-ast) global-environment lexical-environment)
  `(multiple-value-setq
       ,(loop for lhs-ast in (ast:lhs-asts ast)
              collect 
              (translate-ast lhs-ast  global-environment lexical-environment))
     ,(translate-ast (ast:form-ast ast) global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:multiple-value-call-ast) global-environment lexical-environment)
  `(multiple-value-call
       ,(translate-ast (ast:function-form-ast ast) global-environment lexical-environment)
     ,@(loop for form-ast in (ast:form-asts ast)
             collect 
             (translate-ast form-ast  global-environment lexical-environment))))

(defmethod translate-ast
    ((ast ast:values-ast) global-environment lexical-environment)
  `(values
    ,@(loop for argument-ast in (ast:argument-asts ast)
            collect 
            (translate-ast argument-ast  global-environment lexical-environment))))

(defmethod translate-ast
    ((ast ast:multiple-value-prog1-ast) global-environment lexical-environment)
  `(multiple-value-prog1
       ,(translate-ast (ast:first-form-ast ast) global-environment lexical-environment)
     ,@(loop for form-ast in (ast:form-asts ast)
             collect 
             (translate-ast form-ast  global-environment lexical-environment))))
