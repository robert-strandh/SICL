(cl:in-package #:cleavir-generate-ast)

(defmethod cleavir-i18n:report-condition
    ((condition block-name-must-be-a-symbol)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The name of a block must be a symbol,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition situations-must-be-proper-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "EVAL-WHEN situations must be a proper list,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition invalid-eval-when-situation)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "An EVAL-WHEN situation must be one of:~@
           :COMPILE-TOPLEVEL, :LOAD-TOPLEVEL, :EXECUTE, COMPILE, LOAD, EVAL,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition flet-functions-must-be-proper-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The function definitions of an FLET form must be a proper list,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition lambda-must-be-proper-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A LAMBDA expression must be a proper list,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition function-argument-must-be-function-name-or-lambda-expression)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The argument of the special operator FUNCTION must be~@
           a function name or a LAMBDA expression,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition bindings-must-be-proper-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The bindings of a LET or LET* special form must be a proper list,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition binding-must-be-symbol-or-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A binding of a LET or LET* special form must be symbol or a list,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition binding-must-have-length-one-or-two)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A binding of a LET or LET* special form that is a list,~@
           must be a proper list of length 1 or 2,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition variable-must-be-a-symbol)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "In a binding of a LET or LET* special form that is a list,~@
           the first element of that list must be a symbol,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition read-only-p-must-be-boolean)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The second argument of a LOAD-TIME-VALUE special form,~@
           must be a Boolean constant (so T or NIL),~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition block-name-unknown)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "In a RETURN or RETURN-FROM special form, the block name given~@
           must have been established by a BLOCK special form,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition setq-must-have-even-number-of-arguments)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The SETQ special form must have an even number of arguments,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition setq-var-must-be-symbol)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The variable assigned to in a SETQ special form must be a symbol,~@
           but the following was found instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition setq-constant-variable)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The variable assigned to in a SETQ must not be a constant variable,~@
           but the following constant variable was found:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition variable-name-unknown)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A variable referred to must have been previously defined,~@
           using either some global operator such as DEFVAR or DEFPARAMETER,~@
           or some form such as LET or LET* for creating a local variable,~@
           but the following undefined variable was found:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition function-name-unknown)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A function referred to must have been previously defined,~@
           using either some global operator such as DEFUN or DEFGENERIC,~@
           or some form such as FLET or LABELS for creating a local function,~@
           but the following undefined function was found:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition function-name-names-global-macro)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A function name was found in a context where the name~@
           must refer to a global or a local function, but the~@
           name refers to a global macro instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition function-name-names-local-macro)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A function name was found in a context where the name~@
           must refer to a global or a local function, but the~@
           name refers to a local macro instead:~@
           ~s"
	  (expr condition)))

(defmethod cleavir-i18n:report-condition
    ((condition no-default-method)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "Cleavir does not supply methods for compiling every special operator.~@
           In particular, no default method is supplied for the following:~@
           CATCH, THROW, UNWIND-PROTECT, and PROGV.~@
           Client code must either define these operators as macros,~@
           or supply a method on CONVERT-SPECIAL, specialized to the~@
           name of the operator and to the implementation-specific environment.~@
           The following operator was found:~@
           ~s"
	  (expr condition)))
