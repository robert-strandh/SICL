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
