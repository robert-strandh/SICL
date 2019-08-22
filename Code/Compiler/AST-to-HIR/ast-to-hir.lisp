(cl:in-package #:sicl-ast-to-hir)

(defclass top-level-enter-instruction (cleavir-ir:top-level-enter-instruction)
  ((%constants :initform '() :accessor constants)))

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
    (change-class hir 'top-level-enter-instruction)
    (sicl-argument-processing:process-parameters hir)
    (sicl-hir-transformations:convert-symbol-value hir)
    (sicl-hir-transformations:convert-set-symbol-value hir)
    (sicl-hir-transformations:hoist-fdefinitions hir)
    (cleavir-hir-transformations::process-captured-variables hir)
    (sicl-hir-transformations:eliminate-create-cell-instructions hir)
    (sicl-hir-transformations:eliminate-fetch-instructions hir)
    (sicl-hir-transformations:eliminate-read-cell-instructions hir)
    (sicl-hir-transformations:eliminate-write-cell-instructions hir)
    hir))
