(cl:in-package #:sicl-compiler)

(defun tie-code-object (code-object environment)
  (let* ((client (sicl-environment:client environment))
         (sicl-run-time:*dynamic-environment* '())
         (function-cell-function
           (sicl-environment:fdefinition
            client environment 'sicl-data-and-control-flow:function-cell)))
    (loop for call-site in (call-sites code-object)
          for instruction = (instruction call-site)
          when (typep instruction 'sicl-ir:named-call-instruction)
            do (let ((cell (sicl-ir:function-cell-cell instruction))
                     (name (name call-site)))
                 (setf (car cell)
                       (funcall function-cell-function name))))
    (funcall (hir-thunks code-object)
             (apply #'vector
                    ;; FIXME: remove this element.
                    nil
                    ;; FIXME: get these functions from the environment.
                    #'sicl-hir-evaluator:enclose
                    #'sicl-hir-evaluator:initialize-closure
                    #'cons
                    nil
                    (append (loop with names = (function-names code-object)
                                  for name in names
                                  collect (funcall function-cell-function name))
                            (constants code-object))))))
