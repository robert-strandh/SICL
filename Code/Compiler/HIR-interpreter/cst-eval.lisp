(cl:in-package #:sicl-hir-interpreter)

(defgeneric cst-eval (client cst environment))

(defmethod cst-eval (client cst environment)
  (let* ((global-environment (trucler-reference:global-environment environment))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (cleavir-cst-to-ast:cst-to-ast client cst global-environment)))
         (hir (sicl-ast-to-hir:ast-to-hir client ast))
         (fun (top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '()))
    (funcall fun
             (make-function-cell-finder global-environment)
             (apply #'vector
                    nil ; Ultimately, replace with code object.
                    #'enclose
                    #'cons
                    nil
                    (sicl-hir-transformations:constants hir)))))
