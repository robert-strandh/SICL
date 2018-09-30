(cl:in-package #:cleavir-load-time-value-hoisting-test)

(defun hoisted-hir-from-form (lambda-form)
  (let* ((environment *environment*)
         (system (make-instance 'client
                   :environment environment)))
    (cleavir-load-time-value-hoisting:hoist-load-time-values
     (cleavir-ast-to-hir:ast-to-hir
      (cleavir-generate-ast:generate-ast lambda-form environment system))
     system)))
