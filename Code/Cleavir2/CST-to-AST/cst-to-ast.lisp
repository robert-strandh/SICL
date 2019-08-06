(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initform *origin* :initarg :origin :reader origin)))

(cleavir-io:define-save-info ast-mixin
  (:origin origin))

(defun cst-to-ast (client cst environment)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil))
    (if (not *use-file-compilation-sematics-p*)
        (convert client cst environment)
        (let ((*similarity-table* (make-similarity-table))
              (*prologue* '()))
          (let ((top-level-ast (convert client cst environment)))
            (cleavir-ast:make-ast 'cleavir-ast:progn-ast
              :origin (origin top-level-ast)
              :form-asts (append (reverse *prologue*)
                                 (list top-level-ast))))))))
