(cl:in-package #:cleavir-cst-to-ast)

(defmethod acclimation:report-condition
    ((condition malformed-lambda-list)
     stream
     (language acclimation:english))
  (format stream "The lambda list ~s is malformed."
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition incorrect-number-of-arguments)
     stream
     (language acclimation:english))
  (let ((form (cst:raw (cst condition)))
        (min (expected-min condition)) (max (expected-max condition)))
    (format stream
            "In the form ~s, ~s is used with ~d arguments, ~@
             but expects ~@?."
            form (first form) (observed condition)
            (cond ((and min max) "between ~d and ~d")
                  (min "at least ~d")
                  (max "at least ~d")
                  ;; If we hit here it's actually a bug.
                  (t "otherwise"))
            min max)))

(defmethod acclimation:report-condition
    ((condition values-&rest-syntax)
     stream
     (language acclimation:english))
  (format stream "The values type ~s is syntactically invalid:~@
                  it has a &rest variable followed by more elements."
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition ignored-variable-referenced)
     stream
     (language acclimation:english))
  (format stream "The variable ~s was referenced,~@
                  despite being declared ~s."
          (cst:raw (cst condition))
          'ignore))

(defmethod acclimation:report-condition
    ((condition block-name-must-be-a-symbol)
     stream
     (language acclimation:english))
  (format stream
          "The name of a block must be a symbol,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition form-must-be-proper-list)
     stream
     (language acclimation:english))
  (format stream
          "Forms must be proper lists,~@
           but the following was found instead:~%~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition situations-must-be-proper-list)
     stream
     (language acclimation:english))
  (format stream
          "EVAL-WHEN situations must be a proper list,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition invalid-eval-when-situation)
     stream
     (language acclimation:english))
  (format stream
          "An EVAL-WHEN situation must be one of:~@
           :COMPILE-TOPLEVEL, :LOAD-TOPLEVEL, :EXECUTE, COMPILE, LOAD, EVAL,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition local-function-definition-must-be-proper-list)
     stream
     (language acclimation:english))
  (format stream
          "Definitions of local functions must be proper lists,~@
           but the following was found instead:~%~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition lambda-must-be-proper-list)
     stream
     (language acclimation:english))
  (format stream
          "A LAMBDA expression must be a proper list,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition function-argument-must-be-function-name-or-lambda-expression)
     stream
     (language acclimation:english))
  (format stream
          "The argument of the special operator FUNCTION must be~@
           a function name or a LAMBDA expression,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition function-name-must-be-proper-function-name)
     stream
     (language acclimation:english))
  (format stream
          "The names bound by FLET must be valid function names,~@
           but the following was found instead:~%~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition bindings-must-be-proper-list)
     stream
     (language acclimation:english))
  (format stream
          "The bindings of a ~s special form must be a proper list,~@
           but the following was found instead:~@
           ~s"
          (operator condition)
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition binding-must-be-symbol-or-list)
     stream
     (language acclimation:english))
  (format stream
          "A binding of a LET or LET* special form must be symbol or a list,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition binding-must-have-length-one-or-two)
     stream
     (language acclimation:english))
  (format stream
          "A binding of a LET or LET* special form that is a list,~@
           must be a proper list of length 1 or 2,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition variable-must-be-a-symbol)
     stream
     (language acclimation:english))
  (format stream
          "In a binding of a LET or LET* special form that is a list,~@
           the first element of that list must be a symbol,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition read-only-p-must-be-boolean)
     stream
     (language acclimation:english))
  (format stream
          "The second argument of a LOAD-TIME-VALUE special form,~@
           must be a Boolean constant (so T or NIL),~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition setq-must-have-even-number-of-arguments)
     stream
     (language acclimation:english))
  (format stream
          "The SETQ special form must have an even number of arguments,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition setq-var-must-be-symbol)
     stream
     (language acclimation:english))
  (format stream
          "The variable assigned to in a SETQ special form must be a symbol,~@
           but the following was found instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition setq-constant-variable)
     stream
     (language acclimation:english))
  (format stream
          "The variable assigned to in a SETQ must not be a constant variable,~@
           but the following constant variable was found:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition variable-name-unknown)
     stream
     (language acclimation:english))
  (format stream
          "A variable referred to must have been previously defined,~@
           using either some global operator such as DEFVAR or DEFPARAMETER,~@
           or some form such as LET or LET* for creating a local variable,~@
           but the following undefined variable was found:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition function-name-unknown)
     stream
     (language acclimation:english))
  (format stream
          "A function referred to must have been previously defined,~@
           using either some global operator such as DEFUN or DEFGENERIC,~@
           or some form such as FLET or LABELS for creating a local function,~@
           but the following undefined function was found:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition function-name-names-global-macro)
     stream
     (language acclimation:english))
  (format stream
          "A function name was found in a context where the name~@
           must refer to a global or a local function, but the~@
           name refers to a global macro instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition function-name-names-local-macro)
     stream
     (language acclimation:english))
  (format stream
          "A function name was found in a context where the name~@
           must refer to a global or a local function, but the~@
           name refers to a local macro instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition function-name-names-special-operator)
     stream
     (language acclimation:english))
  (format stream
          "A function name was found in a context where the name~@
           must refer to a global or a local function, but the~@
           name refers to a special operator instead:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition no-default-method)
     stream
     (language acclimation:english))
  (format stream
          "Cleavir does not supply methods for compiling every special operator.~@
           In particular, no default method is supplied for the following:~@
           CATCH, THROW, UNWIND-PROTECT, and PROGV.~@
           Client code must either define these operators as macros,~@
           or supply a method on CONVERT-SPECIAL, specialized to the~@
           name of the operator and to the implementation-specific environment.~@
           The following form was found:~@
           ~s"
          (cst:raw (cst condition))))

(defmethod acclimation:report-condition
    ((condition lambda-call-first-symbol-not-lambda)
     stream
     (language acclimation:english))
  (format stream
          "Lambda call form was used with the malformed lambda block~@
          first argument instead of the symbol LAMBDA. The following~@
          form was found:~@
          ~s"
          (cst:raw (cst condition))))

;; Helper for below.
(defmethod acclimation:report-condition
  ((condition too-many-arguments)
   stream
   (language acclimation:english))
  (format stream
          "~s was called with too many arguments:~%~s~@
          Expected at most ~d,"
          (car (cst:raw (cst condition)))
          (cst:raw (cst condition))
          (expected-max condition)))

(defmethod acclimation:report-condition
  ((condition not-enough-arguments)
   stream
   (language acclimation:english))
  (format stream
          "~s was called with too few arguments:~%~s~@
          Expected at least ~d,"
          (car (cst:raw (cst condition)))
          (cst:raw (cst condition))
          (expected-min condition)))

(defmethod acclimation:report-condition
  ((condition odd-keyword-portion)
   stream
   (language acclimation:english))
  (format stream
          "~s was called with an odd number of arguments in the keyword portion:~%~s"
          (car (cst:raw (cst condition)))
          (cst:raw (cst condition))))

;; Display the type declaration that informed us of the problem.
(defmethod acclimation:report-condition :after
    ((condition argument-mismatch-warning)
     stream
     (language acclimation:english))
  (format stream
          " as determined from the function's type,~%~s"
          (callee-ftype condition)))

;; NOTE: In the future, there may be other ways to signal this.
(defmethod acclimation:report-condition :after
    ((condition argument-mismatch-style-warning)
     stream
     (language acclimation:english))
  (format stream
          " as inferred from its inline definition."))

(defmethod acclimation:report-condition
  ((condition macroexpansion-error)
   stream
   (language acclimation:english))
  (format stream
          "ERROR during macroexpansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition macroexpansion-warning)
   stream
   (language acclimation:english))
  (format stream
          "WARNING during macroexpansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition macroexpansion-style-warning)
   stream
   (language acclimation:english))
  (format stream
          "STYLE-WARNING during macroexpansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition compiler-macro-expansion-error)
   stream
   (language acclimation:english))
  (format stream
          "ERROR during compiler-macro-expansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition compiler-macro-expansion-warning)
   stream
   (language acclimation:english))
  (format stream
          "WARNING during compiler-macro-expansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition compiler-macro-expansion-style-warning)
   stream
   (language acclimation:english))
  (format stream
          "STYLE-WARNING during compiler-macro-expansion:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition eval-error)
   stream
   (language acclimation:english))
  (format stream
          "ERROR while evaluating compile-time side effect:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition eval-warning)
   stream
   (language acclimation:english))
  (format stream
          "WARNING while evaluating compile-time side effect:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
  ((condition eval-style-warning)
   stream
   (language acclimation:english))
  (format stream
          "STYLE-WARNING while evaluating compile-time side effect:~%~@<  ~@;~a~:>"
          (original-condition condition)))

(defmethod acclimation:report-condition
    ((condition circular-dependencies-in-creation-form)
     stream
     (language acclimation:english))
  (with-accessors ((object object)
                   (creation-form creation-form))
      condition
    (format stream
            "The creation form ~S~@
             of the object ~S~@
             contains a circular dependency."
            creation-form object)))

(defmethod acclimation:report-condition
    ((condition object-not-externalizable)
     stream
     (language acclimation:english))
  (with-accessors ((object object)) condition
    (format stream
            "The object ~S is not externalizable."
            object)))
