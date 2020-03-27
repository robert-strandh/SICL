(cl:in-package #:sicl-compiler)

(defun compile-ast (client ast environment)
  (declare (ignore environment)) ; for now
  (let ((hir (sicl-ast-to-hir:ast-to-hir client ast)))
    (sicl-hir-transformations:eliminate-append-values-instructions hir)
    (sicl-hir-to-mir:hir-to-mir client hir)
    (sicl-mir-to-lir:mir-to-lir client hir)
    (multiple-value-bind (instructions label-map)
        (cluster:assemble (sicl-code-generation:generate-code hir))
      (declare (ignore label-map))
      (make-instance 'code-object
        :instructions instructions
        :frame-maps nil
        :callee-saves-register-maps nil
        :callee-saved-stack-maps nil))))
