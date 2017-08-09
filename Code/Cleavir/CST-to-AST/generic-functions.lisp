(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (cst environment system))

(defgeneric convert-cst (cst info environment system))

(defgeneric convert-special (head cst environment system))

(defgeneric convert-special-binding
    (variable value-ast next-ast env system))

(defgeneric convert-lambda-call (cst env system))

(defgeneric convert-code (lambda-list body-cst env system &optional block-name))

(defgeneric convert-variable (cst environment system))
