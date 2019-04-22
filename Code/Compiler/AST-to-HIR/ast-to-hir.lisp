(cl:in-package #:sicl-ast-to-hir)

(defun ast-to-hir (ast)
  ;; Wrap the AST in a FUNCTION-AST that will be called at load time
  ;; with a single argument, namely a function that, given a function
  ;; name, returns the function cell from the environment the code
  ;; will be loaded into.
  (let* ((dynamic-environment-out (make-instance 'cleavir-ast:lexical-ast :name 'dummy))
         (wrapped-ast (make-instance 'cleavir-ast:function-ast
                        :lambda-list '()
                        :body-ast ast
                        :dynamic-environment-ast nil
                        :dynamic-environment-out dynamic-environment-out)))
    (cleavir-ast-to-hir:compile-toplevel-unhoisted wrapped-ast)))

