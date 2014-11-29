(cl:in-package #:sicl-standard-environment-functions)

(defmethod cleavir-i18n:report-condition
    ((condition no-such-class) stream (langauge cleavir-i18n:english))
  (format stream "There is no class named ~s" (name condition)))

(defmethod cleavir-i18n:report-condition
    ((condition odd-number-of-arguments-to-setf)
     stream
     (langauge cleavir-i18n:english))
  (format stream 
	  "An odd number of arguments was given to SETF~@
           in the following form:~@
           ~s"
          (form condition)))

(defmethod cleavir-i18n:report-condition
    ((condition variables-must-be-proper-list)
     stream
     (langauge cleavir-i18n:english))
  (format stream
	  "The variables to be bound by MULTIPLE-VALUE-BIND must~@
           be a proper list, but the following was found instead:~@
           ~s"
         (variables condition)))
