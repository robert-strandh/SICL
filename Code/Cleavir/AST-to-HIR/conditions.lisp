(cl:in-package #:cleavir-ast-to-hir)

;;; An AST appears in a context inappropriate for it.
;;; This is a fatal compiler error.
(define-condition miscontext (error)
  ((%ast :initarg :ast :reader ast)
   (%context :initarg :context :reader miscontext-context)
   ;; The AST needs this many results and this many successors.
   ;; NIL is don't-care
   (%ast-results :initarg :ast-results :reader ast-results)
   (%ast-successors :initarg :ast-successors :reader ast-successors))
  (:report (lambda (condition stream)
             (with-accessors ((ast ast) (expected-results ast-results)
                              (expected-successors ast-successors)
                              (context miscontext-context))
                 condition
               (format stream
                       "Error during AST-TO-HIR:~@
                        Found ~a with ~@[~d results ~]~@[~d successors~] in context where ~@[~d results ~]~@[~d successors ~]were expected.~@
                        This is probably caused by a bug in GENERATE-AST."
                       ast
                       (if expected-results
                           (length (results context))
                           nil)
                       (if expected-successors
                           (length (successors context))
                           nil)
                       expected-results expected-successors)))))

;;; FIXME: Check that no results are required when AST-RESULTS is NIL.

(defun assert-context (ast context ast-results ast-successors)
  (when (or (and ast-results
                 (/= ast-results (length (results context))))
            (and ast-successors
                 (/= ast-successors
                     (length (successors context)))))
    (error 'miscontext :ast ast :context context
                       :ast-results ast-results
                       :ast-successors ast-successors)))
