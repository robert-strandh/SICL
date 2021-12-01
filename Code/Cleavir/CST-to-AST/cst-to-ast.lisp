(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initform *origin* :initarg :origin :reader origin)))

(cleavir-io:define-save-info ast-mixin
  (:origin origin))

(defun cst-to-ast (client cst environment &key file-compilation-semantics)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil)
        (*use-file-compilation-semantics-p* file-compilation-semantics))
    (if (not *use-file-compilation-semantics-p*)
        (convert client cst environment)
        (cleavir-literals:with-fresh-similarity-table
          (let ((top-level-ast (convert client cst environment)))
            (cleavir-ast:make-ast 'cleavir-ast:progn-ast
              :origin (origin top-level-ast)
              :form-asts
              (append (cleavir-literals:finalize-literals client environment)
               (list top-level-ast))))))))
