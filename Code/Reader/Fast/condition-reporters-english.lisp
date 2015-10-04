(cl:in-package #:sicl-read)

(defmethod cleavir-i18n:report-condition
    ((condition unmatched-right-parenthesis)
     stream
     (language cleavir-i18n:english))
  (format stream "Unmatched right parenthesis found."))

(defmethod cleavir-i18n:report-condition
    ((condition only-dots-in-token)
     stream
     (language cleavir-i18n:english))
  (format stream "A token with only dots in it was found"))
