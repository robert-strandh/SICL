(in-package #:cleavir-code-utilities)

(defmethod cleavir-i18n:report-condition
    ((condition form-must-be-proper-list) stream (language cleavir-i18n:english))
  (format stream "A form must be a proper list,~@
                  but the following was found instead:~@
                  ~s" (form condition)))
