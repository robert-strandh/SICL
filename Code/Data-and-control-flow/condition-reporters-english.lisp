(cl:in-package #:sicl-data-and-control-flow)

(defmethod cleavir-i18n:report-condition
    ((condition odd-number-of-arguments-to-setf)
     stream
     (langauge cleavir-i18n:english))
  (format stream 
	  "An odd number of arguments was given to SETF~@
           in the following form:~@
           ~s"
          (form condition)))
