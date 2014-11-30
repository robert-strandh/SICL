(cl:in-package #:sicl-standard-environment-functions)

(defmethod cleavir-i18n:report-condition
    ((condition no-such-class) stream (langauge cleavir-i18n:english))
  (format stream "There is no class named ~s" (name condition)))

(defmethod cleavir-i18n:report-condition
    ((condition variables-must-be-proper-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The variables to be bound by MULTIPLE-VALUE-BIND must~@
           be a proper list, but the following was found instead:~@
           ~s"
         (variables condition)))

(defmethod cleavir-i18n:report-condition
    ((condition variable-must-be-symbol)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "A variable to be bound by MULTIPLE-VALUE-BIND must~@
           be a symbol, but the following was found instead:~@
           ~s"
         (variable condition)))
