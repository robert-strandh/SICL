(cl:in-package #:sicl-ast-to-hir)

(defun ast-to-hir (client ast)
    (eliminate-fdefinition-asts ast)
  (let* ((cleavir-cst-to-ast::*origin* nil)
         (parameter-ast
           (make-instance 'cleavir-ast:lexical-ast
             :name (gensym))))
    (multiple-value-bind (hoisted-ast load-time-value-count)
        (hoist-load-time-value ast parameter-ast)
      (declare (ignore load-time-value-count))
      (let* ((wrapped-ast (make-instance 'cleavir-ast:function-ast
                            :lambda-list (list parameter-ast)
                            :body-ast hoisted-ast))
             (hir (cleavir-ast-to-hir:compile-toplevel-unhoisted client wrapped-ast)))
        hir))))
