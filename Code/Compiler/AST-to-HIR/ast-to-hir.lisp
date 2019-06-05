(cl:in-package #:sicl-ast-to-hir)

(defun ast-to-hir (ast)
  ;; Wrap the AST in a FUNCTION-AST that will be called at load time
  ;; with a single argument, namely a function that, given a function
  ;; name, returns the function cell from the environment the code
  ;; will be loaded into.
  (let* ((cleavir-cst-to-ast::*origin* nil)
         (hoisted-ast (cleavir-ast-transformations:hoist-load-time-value ast))
         (wrapped-ast (make-instance 'cleavir-ast:function-ast
                        :lambda-list '()
                        :body-ast hoisted-ast))
         (hir (cleavir-ast-to-hir:compile-toplevel-unhoisted wrapped-ast)))
    (sicl-hir-transformations:convert-symbol-value hir)
    (sicl-hir-transformations:convert-set-symbol-value hir)
    (sicl-hir-transformations:hoist-fdefinitions hir)
    (cleavir-hir-transformations::process-captured-variables hir)
    hir))
