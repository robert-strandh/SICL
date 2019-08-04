(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The base classes for conditions used here.

(define-condition compilation-condition (acclimation:condition)
  ((%cst :initarg :cst :reader cst)))

(define-condition compilation-program-error
    (program-error compilation-condition)
  ())

(define-condition compilation-warning
    (warning compilation-condition)
  ())

(define-condition compilation-style-warning
    (style-warning compilation-condition)
  ())

;;; This class is used for conditions that "encapsulate"
;;; other conditions, for when something out of our control
;;; (e.g. a macroexpander) signals.
(define-condition encapsulated-condition (acclimation:condition)
  ((%original-condition :initarg :condition
                        :reader original-condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; More specific but still general conditions.

(define-condition argument-mismatch
    (acclimation:condition)
  ((%expected-min :initarg :expected-min :reader expected-min)
   (%expected-max :initarg :expected-max :reader expected-max)))

(define-condition argument-mismatch-warning
    (argument-mismatch compilation-warning)
  ;; The type is canonicalized.
  ((%callee-ftype :initarg :callee-ftype :reader callee-ftype)))

(define-condition argument-mismatch-style-warning
    (argument-mismatch compilation-style-warning)
  ;; FIXME: might include a "reason" field for when there's more
  ;; than one possible way to signal this.
  ())

;;; These three are further specialized with the above two below.
(define-condition too-many-arguments
    (argument-mismatch)
  ())

(define-condition not-enough-arguments
    (argument-mismatch)
  ())

;;; The class of conditions signaled when an odd number of
;;; arguments is passed to the &key portion of a lambda list.
(define-condition odd-keyword-portion
    (argument-mismatch)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Specific conditions.

;;; This condition is signaled by a call to ERROR that was introduced
;;; as a replacement of a form that triggered a compilation error.
(define-condition run-time-program-error
    (program-error acclimation:condition)
  ((%expr :initarg :expr :reader expr)
   (%origin :initarg :origin :reader origin)))

;;; This condition is signaled when the number of arguments can be
;;; determined to be incorrect at compile time, i.e. for special
;;; operators, primitive operators, and calls to standard functions.
;;;
;;; EXPECTED-MIN is the minimum number of arguments allowed for the
;;; operator and it is a non-negative integer.  EXPECTED-MAX is the
;;; maximum number of arguments allowed for this operator, and is
;;; either a non-negative integer, or NIL, meaning that the operator
;;; can take an arbitrary number of arguments.  OBSERVED is the number
;;; of arguments actually supplied.
(define-condition incorrect-number-of-arguments
    (compilation-program-error)
  ((%expected-min :initarg :expected-min :reader expected-min)
   (%expected-max :initarg :expected-max :reader expected-max)
   (%observed :initarg :observed :reader observed)))

;;; This condition is signaled when a values type has erroneous
;;; syntax around &rest.
(define-condition values-&rest-syntax
    (compilation-program-error)
  ())

;;; This condition is signaled when a variable declared IGNORE is
;;; nonetheless used.
(define-condition ignored-variable-referenced
    (compilation-style-warning)
  ())

;;; This condition is signaled when the first argument to BLOCK or
;;; RETURN-FROM is not a symbol.
(define-condition block-name-must-be-a-symbol
    (compilation-program-error)
  ())

;;; This condition is signaled when a form that must be a proper list
;;; in fact is not.
(define-condition form-must-be-proper-list
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

;;; This condition is signaled when a local function definition is not
;;; a proper list.
(define-condition local-function-definition-must-be-proper-list
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

;;; This condition is signaled when the name of a local function
;;; definition is not a proper function name (i.e., neither a symbol,
;;; nor a list of the form (SETF symbol))
(define-condition function-name-must-be-proper-function-name
    (compilation-program-error)
  ())

;;; This condition is signaled when the first argument of a LET or a
;;; LET* form (i.e., the bindings) is not a proper list.
(define-condition bindings-must-be-proper-list
    (compilation-program-error)
  ((%operator :initarg :operator :reader operator)))

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

;;; This condition is signaled when a function name is encountered
;;; during compilation in a context where the name must be that of a
;;; local or a global function, but instead the name has a definition
;;; as a global macro in the environment in which the function name is
;;; compiled.
(define-condition function-name-names-global-macro
    (compilation-program-error)
  ())

;;; This condition is signaled when a function name is encountered
;;; during compilation in a context where the name must be that of a
;;; local or a global function, but instead the name has a definition
;;; as a local macro in the environment in which the function name is
;;; compiled.
(define-condition function-name-names-local-macro
    (compilation-program-error)
  ())

;;; This condition is signaled when a function name is encountered
;;; during compilation in a context where the name must be that of a
;;; local or a global function, but instead the name has a definition
;;; as a special operator in the environment in which the function
;;; name is compiled.
(define-condition function-name-names-special-operator
    (compilation-program-error)
  ())

;;; This condition is signaled by methods on CONVERT-SPECIAL,
;;; specialized to operators for which Cleavir does not provide a
;;; default method.
;;; It is not a compilation-program-error because the source code
;;; could be fine- this is instead an issue with the client, and
;;; very likely a bug in its use of Cleavir.
(define-condition no-default-method
    (error acclimation:condition)
  ((%cst :initarg :cst :reader cst)
   (%operator :initarg :operator :reader operator)))

;;; This condition is signaled when a LAMBDA-CALL expression is
;;; encountered, but the first symbol isn't LAMBDA.
(define-condition lambda-call-first-symbol-not-lambda
    (compilation-program-error)
  ())

;;; This condition is signaled when a lambda list is malformed.
(define-condition malformed-lambda-list (compilation-program-error)
  ())

;;; This condition is signaled when a function of known type is
;;; called with too many arguments.
(define-condition too-many-arguments-warning
    (too-many-arguments argument-mismatch-warning)
  ())

;;; This condition is called when a function is called with too
;;; many arguments, but we only know that for other reasons. For
;;; example, from an inline definition. Technically the programmer
;;; could redefine the function to make the call okay.
(define-condition too-many-arguments-style-warning
    (too-many-arguments argument-mismatch-style-warning)
  ())

;;; This condition is signaled when a function of known type is
;;; called with too few arguments.
(define-condition not-enough-arguments-warning
    (not-enough-arguments argument-mismatch-warning)
  ())

;;; See too-many-arguments-style-warning.
(define-condition not-enough-arguments-style-warning
    (not-enough-arguments argument-mismatch-style-warning)
  ())

;;; See odd-keyword-portion.
(define-condition odd-keyword-portion-warning
    (odd-keyword-portion argument-mismatch-warning)
  ())

(define-condition odd-keyword-portion-style-warning
    (odd-keyword-portion argument-mismatch-style-warning)
  ())

(define-condition circular-dependencies-in-creation-form
    (compilation-program-error acclimation:condition)
  ((%object :initarg :object :reader object)
   (%creation-form :initarg :creation-form :reader creation-form)))

(define-condition object-not-externalizable
    (compilation-program-error acclimation:condition)
  ((%object :initarg :object :reader object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Encapsulation conditions.

;;; These two functions create condition handlers.
;;; The handlers take the condition they are given, and signal
;;; a new condition that encapsulates but also has source
;;; information from the CST.
;;; They also allow higher level handlers to use the
;;; SIGNAL-ORIGINAL-CONDITION restart to allow the original
;;; condition to propagate instead.
(defun warning-encapsulator (cst condition-type)
  (lambda (condition)
    (let ((muffle t))
      (restart-case
          (warn condition-type
                :cst cst
                :condition condition)
        (signal-original-condition ()
          :report "Let the originally signaled condition propagate."
          (setf muffle nil)))
      (when muffle
        (muffle-warning condition)))))

(defun error-encapsulator (cst condition-type)
  (lambda (condition)
    (restart-case
        (error condition-type
               :cst cst
               :condition condition)
      (signal-original-condition ()
        :report "Let the originally signaled condition propagate."))))

;;; Helper macro. Establishes a handler-bind that wraps
;;; caught conditions in the given classes.
(defmacro with-encapsulated-conditions
    ((cst error-type warning-type style-warning-type) &body body)
  (let ((cstg (gensym "CST")))
    `(let ((,cstg ,cst))
       (handler-bind
           ((style-warning
              (warning-encapsulator ,cstg ',style-warning-type))
            ((and warning (not style-warning))
              (warning-encapsulator ,cstg ',warning-type))
            (error (error-encapsulator ,cstg ',error-type)))
         ,@body))))

;;; This condition is signaled when a macroexpander signals
;;; an error.
(define-condition macroexpansion-error
    (compilation-program-error encapsulated-condition)
  ())

;;; This condition is signaled when a macroexpander signals
;;; a warning.
(define-condition macroexpansion-warning
    (compilation-warning encapsulated-condition)
  ())

;;; This condition is signaled when a macroexpander signals
;;; a style-warning.
(define-condition macroexpansion-style-warning
    (compilation-style-warning encapsulated-condition)
  ())

;;; This condition is signaled when a compiler-macroexpander signals
;;; an error.
(define-condition compiler-macro-expansion-error
    (compilation-program-error encapsulated-condition)
  ())

;;; This condition is signaled when a compiler-macroexpander signals
;;; a warning.
(define-condition compiler-macro-expansion-warning
    (compilation-warning encapsulated-condition)
  ())

;;; This condition is signaled when a compiler-macroexpander signals
;;; a style-warning.
(define-condition compiler-macro-expansion-style-warning
    (compilation-style-warning encapsulated-condition)
  ())

;;; This condition is signaled when a compile-time side-effect signals
;;; an error.
(define-condition eval-error
    (compilation-program-error encapsulated-condition)
  ())

;;; This condition is signaled when a compile-time side-effect signals
;;; a warning.
(define-condition eval-warning
    (compilation-warning encapsulated-condition)
  ())

;;; This condition is signaled when a compile-time side-effect signals
;;; a style warning.
(define-condition eval-style-warning
    (compilation-style-warning encapsulated-condition)
  ())
