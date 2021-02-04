(cl:in-package #:sicl-compiler)

(defgeneric cst-eval (client cst environment))

(defmethod cst-eval (client cst environment)
  (let* ((global-environment (trucler:global-environment client environment))
         (client (sicl-environment:client global-environment))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (cleavir-cst-to-ast:cst-to-ast client cst global-environment)))
         (code-object (compile-ast client ast)))
    (tie-code-object code-object global-environment)))
