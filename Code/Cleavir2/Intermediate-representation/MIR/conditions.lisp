(cl:in-package #:cleavir-ir)

(define-condition input-inputs-mutually-exclusive (error)
  ())

(define-condition output-must-be-given (error)
  ())

(define-condition all-individual-inputs-must-be-given (error)
  ())

(define-condition successor-successors-mutually-exclusive (error)
  ())

(define-condition all-or-no-individual-successors-must-be-given (error)
  ())

(define-condition successor1-must-be-given (error)
  ())
