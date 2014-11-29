(cl:in-package #:sicl-standard-environment-functions)

(defmethod cleavir-i18n:report-condition
    ((condition no-such-class) stream (langauge cleavir-i18n:english))
  (format stream "There is no class named ~s" (name condition)))
