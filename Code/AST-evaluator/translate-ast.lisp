(cl:in-package #:sicl-ast-evaluator)

(defgeneric translate-ast (ast global-environment lexical-environment))

(defmethod translate-ast
    ((ast ast:fdefinition-ast) global-environment lexical-environment)
  `(env:fdefinition
    ,(translate-ast
      (ast:name-ast ast) global-environment lexical-environment)
    ,global-environment))

(defmethod translate-ast
    ((ast ast:constant-ast) global-environment lexical-environment)
  `',(ast:value ast))

(defmethod translate-ast
    ((ast ast:lexical-ast) global-environment lexical-environment)
  (find-lexical-variable ast lexical-environment))

(defmethod translate-ast
    ((ast ast:symbol-value-ast) global-environment lexical-environment)
  `(symbol-value
    ,(translate-ast
      (ast:name-ast ast) global-environment lexical-environment)
    ,global-environment))

(defmethod translate-ast
    ((ast ast:set-symbol-value-ast) global-environment lexical-environment)
  `(setf (symbol-value
          ,(translate-ast
            (ast:name-ast ast) global-environment lexical-environment)
          ,global-environment)
         ,(translate-ast
           (ast:value-ast ast) global-environment lexical-environment)))

(defmethod translate-ast
    ((ast ast:call-ast) global-environment lexical-environment)
  `(funcall
    ,(translate-ast (ast:callee-ast ast) global-environment lexical-environment)
    ,@(loop for argument-ast in (cleavir-ast:argument-asts ast)
            collect (translate-ast
                     argument-ast global-environment lexical-environment))))
