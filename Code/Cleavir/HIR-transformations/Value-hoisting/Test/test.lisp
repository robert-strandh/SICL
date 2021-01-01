(cl:in-package #:cleavir-value-hoisting-test)

(defun hoisted-hir-from-form (form environment)
  (let* ((client (make-instance 'client))
         (*make-string*
           (cleavir-ir:make-lexical-location '%make-string))
         (*cons*
           (cleavir-ir:make-lexical-location '%cons))
         (*find-package*
           (cleavir-ir:make-lexical-location '%find-package))
         (*intern*
           (cleavir-ir:make-lexical-location '%intern))
         (*function-cell*
           (cleavir-ir:make-lexical-location '%function-cell))
         (hir (hir-from-form form client environment)))
    (cleavir-value-hoisting:hoist-values hir client environment)
    (setf (cleavir-ir:lambda-list hir)
          '(%make-string
            %cons
            %find-package
            %intern
            %function-cell))
    (setf (cleavir-ir:outputs hir)
          (append (cleavir-ir:outputs hir)
                  (list *make-string*
                        *cons*
                        *find-package*
                        *intern*
                        *function-cell*)))
    (cleavir-ir:reinitialize-data hir)
    hir))
