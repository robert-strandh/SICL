(in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The base classes for conditions used here. 

(define-condition compilation-program-error (program-error)
  ((%expr :initarg :expr :reader expr)))

(define-condition compilation-warning (warning)
  ((%expr :initarg :expr :reader expr)))

(define-condition compilation-style-warning (style-warning)
  ((%expr :initarg :expr :reader expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specific conditions.

(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

(define-condition situations-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition invalid-eval-when-situation
    (compilation-program-error)
  ())

(define-condition flet-functions-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition functions-body-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition lambda-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition function-argument-must-be-function-name-or-lambda-expression
    (compilation-program-error)
  ())

(define-condition bindings-must-be-proper-list
    (compilation-program-error)
  ())

(define-condition binding-must-be-symbol-or-list
    (compilation-program-error)
  ())

(define-condition binding-must-have-length-one-or-two
    (compilation-program-error)
  ())

(define-condition variable-must-be-a-symbol
    (compilation-program-error)
  ())    

(define-condition load-time-value-must-have-one-or-two-arguments
    (compilation-program-error)
  ())

(define-condition read-only-p-must-be-boolean
    (compilation-program-error)
  ())

(define-condition block-name-unknown
    (compilation-program-error)
  ())

(define-condition setq-must-have-even-number-of-arguments
    (compilation-program-error)
  ())

(define-condition setq-var-must-be-symbol
    (compilation-program-error)
  ())

(define-condition setq-unknown-variable
    (compilation-program-error)
  ())

(define-condition setq-constant-variable
    (compilation-program-error)
  ())
