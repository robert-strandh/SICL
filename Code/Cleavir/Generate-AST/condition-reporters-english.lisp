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
