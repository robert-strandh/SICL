(cl:in-package #:sicl-ast-to-hir)

(defun ast-to-hir (client ast)
    (eliminate-fdefinition-asts ast)
  (let ((cleavir-cst-to-ast::*origin* nil))
    (multiple-value-bind (hoisted-ast load-time-value-count)
        (hoist-load-time-value ast)
      (let* ((wrapped-ast (make-instance 'cleavir-ast:function-ast
                            :lambda-list '()
                            :body-ast hoisted-ast))
             (hir (cleavir-ast-to-hir:compile-toplevel-unhoisted client wrapped-ast))
             (constants (make-array load-time-value-count
                                    :adjustable t :fill-pointer t)))
        (cleavir-partial-inlining:do-inlining hir)
        (sicl-argument-processing:process-parameters hir)
        (sicl-hir-transformations:eliminate-fixed-to-multiple-instructions hir)
        (sicl-hir-transformations:eliminate-multiple-to-fixed-instructions hir)
        (cleavir-hir-transformations::process-captured-variables hir)
        ;; Replacing aliases does not appear to have a great effect when
        ;; code generation is disabled.  Try removing this commented line
        ;; when code generation is again enabled.
        ;; (cleavir-hir-transformations:replace-aliases hir)
        (sicl-hir-transformations:eliminate-create-cell-instructions hir)
        (sicl-hir-transformations:eliminate-fetch-instructions hir)
        (sicl-hir-transformations:eliminate-read-cell-instructions hir)
        (sicl-hir-transformations:eliminate-write-cell-instructions hir)
        (process-constant-inputs hir constants)
        (cleavir-remove-useless-instructions:remove-useless-instructions hir)
        hir))))
