(cl:in-package #:sicl-data-and-control-flow)

(define-compiler-macro shiftf (&whole form)
  (cond ((not (cleavir-code-utilities:proper-list-p form))
         (error 'cleavir-code-utilities:form-must-be-proper-list
                :form form))
        ((< (length form) 3)
         (error 'too-few-arguments-to-shiftf
                :form form))
        (t
         form)))
