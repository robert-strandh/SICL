(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initform *origin* :initarg :origin :reader origin)))

(cleavir-io:define-save-info ast-mixin
  (:origin origin))

(defun cst-to-ast (client cst lexical-environment)
  (let ((*subforms-are-top-level-p* t)
        (*compile-time-too* nil)
        (trucler-environment
          (make-instance 'trucler-reference:environment
            :global-environment lexical-environment)))
    (if (not *use-file-compilation-sematics-p*)
        (convert client cst trucler-environment)
        (let ((*similarity-table* (make-similarity-table))
              (*prologue* '()))
          (let ((top-level-ast (convert client cst trucler-environment)))
            (cleavir-ast:make-ast 'cleavir-ast:progn-ast
              :origin (origin top-level-ast)
              :form-asts (append (reverse *prologue*)
                                 (list top-level-ast))))))))
