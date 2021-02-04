(defun expression-to-mir (expression environment)
  (let* ((client (make-instance 'sicl-boot::client))
         (cst (cst:cst-from-expression expression))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (handler-bind
                    ((trucler:no-function-description
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'cleavir-cst-to-ast:consider-global)))
                     (trucler:no-variable-description
                       (lambda (condition)
                         (declare (ignore condition))
                         (invoke-restart 'cleavir-cst-to-ast:consider-special))))
                  (cleavir-cst-to-ast:cst-to-ast
                   client cst environment
                   :file-compilation-semantics t))))
         (code-object (sicl-compiler:compile-ast client ast))
         (hir (sicl-compiler:ir code-object))
         (mir (sicl-hir-to-mir:hir-to-mir client hir)))
    mir))
