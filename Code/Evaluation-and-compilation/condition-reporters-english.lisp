(cl:in-package #:sicl-evaluation-and-compilation)

(defmethod cleavir-i18n:report-condition
    ((condition environment-must-be-omitted-or-nil)
     stream
     (language cleavir-i18n:english))
  (format stream
	  "The optional environment argument must either be~@
           omitted or NIL, but the following was found instead:~%~s"
	  (environment condition)))
