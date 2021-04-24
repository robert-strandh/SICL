(cl:in-package #:sicl-data-and-control-flow)

(define-compiler-macro psetf (&whole form)
  (cond ((not (cleavir-code-utilities:proper-list-p form))
         (error 'cleavir-code-utilities:form-must-be-proper-list
                :key form))
        ((oddp (length (rest form)))
         (error 'odd-number-of-arguments-to-psetf
                :form form))
        (t
         form)))
