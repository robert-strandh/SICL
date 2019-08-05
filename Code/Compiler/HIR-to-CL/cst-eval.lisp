(cl:in-package #:sicl-hir-to-cl)

(defgeneric cst-eval (client cst environment))

(defmethod cst-eval (client cst environment)
  (let* ((global-environment (trucler-reference:global-environment environment))
         (ast (let ((cleavir-cst-to-ast::*origin* nil)
                    (cleavir-cst-to-ast::*use-file-compilation-sematics-p* nil))
                (cleavir-cst-to-ast:cst-to-ast client cst global-environment)))
         (hir (sicl-ast-to-hir:ast-to-hir ast))
         (cl (hir-to-cl client hir))
         (fun (compile nil cl))
         (*dynamic-environment* '()))
    (funcall fun (make-function-cell-finder global-environment))))
