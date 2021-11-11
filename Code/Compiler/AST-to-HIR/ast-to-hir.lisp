(cl:in-package #:sicl-ast-to-hir)

(defun ast-to-hir (client ast)
  (let* ((cleavir-cst-to-ast::*origin* nil)
         (*gensym-counter* 0)
         (ast (make-instance 'cleavir-ast:progn-ast
                :form-asts (list ast))))
    ;; Eliminating FDEFINITION-ASTs will create LOAD-TIME-VALUE-ASTs, so
    ;; we need to host LOAD-TIME-VALUE-ASTs after FDEFINITION-ASTs have
    ;; been eliminated.
    (eliminate-fdefinition-asts ast)
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
        (sicl-hir-transformations:eliminate-create-cell-instructions hir)
        (sicl-hir-transformations:eliminate-fetch-instructions hir)
        (sicl-hir-transformations:eliminate-read-cell-instructions hir)
        (sicl-hir-transformations:eliminate-write-cell-instructions hir)
        (cleavir-hir-transformations:eliminate-catches hir)
        (process-constant-inputs hir constants)
        (cleavir-remove-useless-instructions:remove-useless-instructions hir)
        (cleavir-hir-transformations:replace-aliases hir)
        (values hir constants)))))
