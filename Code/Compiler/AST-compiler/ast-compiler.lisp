(cl:in-package #:sicl-compiler)

(defun compile-ast (client ast)
  (let ((hir1 (sicl-ast-to-hir:ast-to-hir client ast))
        #+(or)(hir2 (sicl-ast-to-hir:ast-to-hir client ast)))
    ;; (sicl-hir-transformations:eliminate-append-values-instructions hir2)
    ;; (sicl-hir-to-mir:hir-to-mir client hir2)
    ;; (sicl-mir-to-lir:mir-to-lir client hir2)
    (multiple-value-bind (instructions label-map)
        (values nil nil)
        ;; (cluster:assemble (sicl-code-generation:generate-code hir2))
      (declare (ignore label-map))
      (make-instance 'code-object
        :instructions instructions
        :frame-maps nil
        :callee-saves-register-maps nil
        :callee-saves-stack-maps nil
        :constants (sicl-hir-transformations:constants hir1)
        :function-names (sicl-hir-transformations:function-names hir1)
        :hir hir1))))
