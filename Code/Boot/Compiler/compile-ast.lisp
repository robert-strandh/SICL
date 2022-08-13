(cl:in-package #:sicl-boot-compile-and-tie)

(defun compile-ast (client ast)
  (multiple-value-bind (ir literals)
      (sicl-ast-to-hir:ast-to-hir client ast)
    (let ((call-sites (establish-call-sites ir)))
      (change-class ir 'sicl-ir:top-level-enter-instruction
                    :literals literals
                    :call-sites call-sites)
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
        (values call-sites hir-thunks)))))

(defun source-position-equal (p1 p2)
  (and (eql (sicl-source-tracking:line-index (car p1))
            (sicl-source-tracking:line-index (car p2)))
       (eql (sicl-source-tracking:line-index (cdr p1))
            (sicl-source-tracking:line-index (cdr p2)))
       (eql (sicl-source-tracking:character-index (car p1))
            (sicl-source-tracking:character-index (car p2)))
       (eql (sicl-source-tracking:character-index (cdr p1))
            (sicl-source-tracking:character-index (cdr p2)))
       (equalp (sicl-source-tracking:lines (car p1))
               (sicl-source-tracking:lines (car p2)))))

(defun tie (client environment call-sites hir-thunks)
  (let ((sicl-run-time:*dynamic-environment* '())
        (function-cell-function
          (env:fdefinition
           client environment 'sicl-data-and-control-flow:function-cell))
        (who-calls-information
          (env:who-calls-information environment)))
    (loop for call-site in call-sites
          for instruction = (instruction call-site)
          when (typep instruction 'sicl-ir:named-call-instruction)
            do (let ((cell (sicl-ir:function-cell-cell instruction))
                     (name (sicl-compiler:name call-site)))
                 (let ((origin (cleavir-ast-to-hir:origin instruction)))
                   (unless (null origin)
                     (pushnew origin (gethash name who-calls-information '())
                              :test #'source-position-equal)))
                 (setf (car cell)
                       (funcall function-cell-function name))))
    (funcall hir-thunks)))

(defun compile-and-tie (client environment ast)
  (multiple-value-bind (call-sites hir-thunks)
      (compile-ast client ast)
    (tie client environment call-sites hir-thunks)))
