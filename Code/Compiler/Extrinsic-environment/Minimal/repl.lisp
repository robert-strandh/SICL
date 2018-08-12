(cl:in-package #:sicl-minimal-extrinsic-environment)

(defun repl (env1 env2 system)
  (loop do (princ "SICL: ")
           (finish-output)
           (setf *dynamic-environment* '())
           (let ((cst (eclector.concrete-syntax-tree:cst-read)))
             (when (equal (cst:raw cst) '(quit))
               (loop-finish))
             (let ((values (multiple-value-list
                            (cleavir-env:cst-eval cst env1 env2 system))))
               (loop for value in values
                     do (print value))
               (terpri)))))
