(in-package #:cleavir-generate-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The base classes for conditions used here. 

(define-condition compilation-program-error
    (program-error cleavir-i18n:condition)
  ((%expr :initarg :expr :reader expr)))

(define-condition compilation-warning
    (warning cleavir-i18n:condition)
  ((%expr :initarg :expr :reader expr)))

(define-condition compilation-style-warning
    (cleavir-i18n:condition style-warning)
  ((%expr :initarg :expr :reader expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specific conditions.

;;; This condition is signaled when the first argument to BLOCK or
;;; RETURN-FROM is not a symbol.
(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

;;; This condition is signaled when the first argument to EVAL-WHEN is
;;; not a proper list.
(define-condition situations-must-be-proper-list
    (compilation-program-error)
  ())

;;; This condition is signaled when the first argument to EVAL-WHEN
;;; contains an invalid situation.
(define-condition invalid-eval-when-situation
    (compilation-program-error)
  ())

;;; This condition is signaled when the bindings of an FLET form are
;;; not a proper list.
(define-condition flet-functions-must-be-proper-list
    (compilation-program-error)
  ())

;;; This condition is signaled when a LAMBDA expression is
;;; encountered, but it is not a proper list.
(define-condition lambda-must-be-proper-list
    (compilation-program-error)
  ())

;;; This condition is signaled when the argument of FUNCTION is
;;; neither a proper function name (i.e., a symbol, or a list of the
;;; form (SETF symbol)), nor a list starting with LAMBDA.
(define-condition function-argument-must-be-function-name-or-lambda-expression
    (compilation-program-error)
  ())

;;; This condition is signaled when the first argument of a LET or a
;;; LET* form (i.e., the bindings) is not a proper list.
(define-condition bindings-must-be-proper-list
    (compilation-program-error)
  ())

;;; This condition is signaled when a binding of a LET or a LET* form
;;; is neither a symbol nor a list.
(define-condition binding-must-be-symbol-or-list
    (compilation-program-error)
  ())

;;; This condition is signaled when a binding of a LET or LET* form is
;;; a list, but it is not a proper list, or it is a proper list, but
;;; it has a length other than 1 or 2.
(define-condition binding-must-have-length-one-or-two
    (compilation-program-error)
  ())

;;; This condition is signaled when a binding of a LET or LET* form is
;;; a proper list of length 1 or 2, but the first element of that list
;;; is not a symbol. 
(define-condition variable-must-be-a-symbol
    (compilation-program-error)
  ())    

;;; This condition is signaled when LOAD-TIME-VALUE is given an
;;; optional READ-ONLY-P argument, but that argument is neither T nor
;;; NIL.  Notice that the HyperSpec requires this argument to be a
;;; Boolean, and not a generalized Boolean.
(define-condition read-only-p-must-be-boolean
    (compilation-program-error)
  ())

;;; This condition is signaled when the first argument of RETURN-FROM
;;; is a symbol, but there is no BLOCK form with that name in the
;;; current context.
(define-condition block-name-unknown
    (compilation-program-error)
  ())

;;; This condition is signaled when a SETQ form is encountered, but it
;;; does not have an even number of arguments.
(define-condition setq-must-have-even-number-of-arguments
    (compilation-program-error)
  ())

;;; This condition is signaled when a SETQ form is encountered, but 
;;; one of the variables assigned to is not a symbol.
(define-condition setq-var-must-be-symbol
    (compilation-program-error)
  ())

;;; This condition is signaled when a SETQ form is encountered, but 
;;; one of the variables assigned to is a a constant variable. 
(define-condition setq-constant-variable
    (compilation-program-error)
  ())

;;; This condition is signaled when a symbol in a variable position is
;;; encountered during compilation, but it does not have a definition
;;; in the environment in which the symbol is compiled.
(define-condition variable-name-unknown
    (compilation-program-error)
  ())

;;; This condition is signaled when a function name is encountered
;;; during compilation, but it does not have a definition in the
;;; environment in which the function name is compiled.
(define-condition function-name-unknown
    (compilation-program-error)
  ())

;;; This condition is signaled by methods on CONVERT-SPECIAL,
;;; specialized to operators for which Cleavir does not provide a
;;; default method.
(define-condition no-default-method
    (compilation-program-error)
  ((%operator :initarg :operator :reader operator)))
