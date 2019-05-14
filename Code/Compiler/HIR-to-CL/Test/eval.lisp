(cl:in-package #:sicl-hir-to-cl-test)

(defgeneric eval (client form environment))

(defmethod eval (client form environment)
  (let* ((cst (cst:cst-from-expression form))
         (ast (let ((cleavir-cst-to-ast::*origin* nil))
                (cleavir-cst-to-ast:cst-to-ast client cst environment)))
         (hir (sicl-ast-to-hir:ast-to-hir ast))
         (cl (sicl-hir-to-cl:hir-to-cl client hir))
         (fun (compile nil cl))
         (sicl-hir-to-cl::*dynamic-environment* '()))
    (funcall fun (sicl-hir-to-cl::function-finder environment))))
