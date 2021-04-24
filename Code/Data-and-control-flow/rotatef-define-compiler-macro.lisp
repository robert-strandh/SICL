(cl:in-package #:sicl-data-and-control-flow)

(define-compiler-macro rotatef (&whole form)
  (if (not (cleavir-code-utilities:proper-list-p form))
      (error 'cleavir-code-utilities:form-must-be-proper-list
             :form form)
      form))
