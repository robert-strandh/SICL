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
    (funcall (hir-thunks code-object) nil)))
