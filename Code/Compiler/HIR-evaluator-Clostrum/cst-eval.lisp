(cl:in-package #:sicl-hir-evaluator)

(defgeneric cst-eval (client cst environment))

(defmethod cst-eval (client cst environment)
  (let* ((global-environment (trucler:global-environment client environment))
         (client (env:client global-environment))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (cleavir-cst-to-ast:cst-to-ast client cst global-environment)))
         (hir (sicl-ast-to-hir:ast-to-hir client ast))
         (fun (top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '()))
    (funcall fun
             (apply #'vector
                    nil ; Ultimately, replace with code object.
                    #'enclose
                    #'initialize-closure
                    #'cons
                    nil
                    (append (loop with names = (sicl-hir-transformations:function-names hir)
                                  for name in names
                                  collect (env:function-cell client global-environment name))
                            (sicl-hir-transformations:constants hir))))))
