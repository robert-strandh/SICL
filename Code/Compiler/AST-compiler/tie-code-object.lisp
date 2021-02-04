(cl:in-package #:sicl-compiler)

(defun tie-code-object (code-object environment)
  (let* ((client (sicl-environment:client environment))
         (sicl-run-time:*dynamic-environment* '())
         (function-cell-function
           (sicl-environment:fdefinition
            client environment 'sicl-data-and-control-flow:function-cell))
         (hir (ir code-object)))
    (funcall (hir-thunks code-object)
             (apply #'vector
                    ;; FIXME: remove this element.
                    nil
                    ;; FIXME: get these functions from the environment.
                    #'sicl-hir-evaluator:enclose
                    #'sicl-hir-evaluator:initialize-closure
                    #'cons
                    nil
                    (append (loop with names = (sicl-hir-transformations:function-names hir)
                                  for name in names
                                  collect (funcall function-cell-function name))
                            (sicl-hir-transformations:constants hir))))))
