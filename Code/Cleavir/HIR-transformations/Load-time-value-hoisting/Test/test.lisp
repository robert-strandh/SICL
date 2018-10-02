(cl:in-package #:cleavir-load-time-value-hoisting-test)

(defun hoisted-hir-from-form (form)
  (let* ((environment *environment*)
         (system (make-instance 'client
                   :environment environment))
         (*make-string*
           (cleavir-ir:make-lexical-location '%make-string))
         (*cons*
           (cleavir-ir:make-lexical-location '%cons))
         (*find-package*
           (cleavir-ir:make-lexical-location '%find-package))
         (*intern*
           (cleavir-ir:make-lexical-location '%intern))
         (*function-cell*
           (cleavir-ir:make-lexical-location 'sicl-genv:function-cell)))
    (let ((hir (cleavir-ast-to-hir:compile-toplevel-unhoisted
                (cleavir-generate-ast:generate-ast
                 `(lambda () ,form) environment system))))
      (cleavir-load-time-value-hoisting:hoist-load-time-values hir system)
      (setf (cleavir-ir:lambda-list hir)
            '(%make-string
              %cons
              %find-package
              %intern
              sicl-genv:function-cell))
      (setf (cleavir-ir:outputs hir)
            (append (cleavir-ir:outputs hir)
                    (list *make-string*
                          *cons*
                          *find-package*
                          *intern*
                          *function-cell*)))
      (cleavir-ir:reinitialize-data hir)
      hir)))
