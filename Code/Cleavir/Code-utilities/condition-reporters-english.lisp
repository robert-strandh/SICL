(cl:in-package #:cleavir-code-utilities)

(defmethod cleavir-i18n:report-condition
    ((condition form-must-be-proper-list) stream (language cleavir-i18n:english))
  (format stream "A form must be a proper list,~@
                  but the following was found instead:~@
                  ~s" (form condition)))

(defmethod cleavir-i18n:report-condition
    ((condition invalid-number-of-arguments)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "Invalid number of arguments.~@
           There must be at least ~d arguments.~%"
	  (min-argcount condition))
  (unless (null (max-argcount condition))
    (format stream
	    "And there must be at most ~d arguments.~%"
	    (max-argcount condition)))
  (format stream
	  "The form that causes the problem was:~%~s"
	  (form condition)))
