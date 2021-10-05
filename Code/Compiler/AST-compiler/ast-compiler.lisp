(cl:in-package #:sicl-compiler)

(defun compile-ast (client ast)
  (multiple-value-bind (hir constants)
      (sicl-ast-to-hir:ast-to-hir client ast)
    (let ((code-object
            (make-instance 'code-object
              :constants constants
              :ast ast
              :ir hir)))
      (establish-call-sites code-object)
      (setf (hir-thunks code-object)
            (sicl-hir-evaluator:top-level-hir-to-host-function client hir))
      (sicl-hir-transformations:eliminate-append-values-instructions hir)
      (sicl-hir-to-mir:hir-to-mir client code-object)
      (sicl-mir-to-lir:mir-to-lir client hir)
      ;; Not sure why this one is necessary.  Sometime before this
      ;; stage, there is an instruction I1 that has a successor I2,
      ;; but I1 is not a predecessor of I2.
      (cleavir-ir:set-predecessors hir)
      (sicl-code-generation:generate-code hir)
      (cluster:assemble (sicl-code-generation:generate-code hir))
      code-object)))
