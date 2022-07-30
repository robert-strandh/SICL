(cl:in-package #:sicl-compiler)

(defun compile-ast (client ast)
  (multiple-value-bind (ir literals)
      (sicl-ast-to-hir:ast-to-hir client ast)
    (let ((code-object
            (make-instance 'code-object
              :literals literals
              :call-sites (establish-call-sites ir))))
      (let ((hir-thunks
              (sicl-hir-evaluator:top-level-hir-to-host-function client ir)))
        (sicl-hir-to-mir:hir-to-mir client ir literals)
        (sicl-mir-to-lir:mir-to-lir client ir)
        ;; Not sure why this one is necessary.  Sometime before this
        ;; stage, there is an instruction I1 that has a successor I2,
        ;; but I1 is not a predecessor of I2.
        (cleavir-ir:set-predecessors ir)
        (sicl-code-generation:generate-code ir)
        (cluster:assemble (sicl-code-generation:generate-code ir))
        (values code-object hir-thunks)))))
