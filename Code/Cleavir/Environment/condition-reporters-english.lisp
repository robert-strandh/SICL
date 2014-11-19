(cl:in-package #:cleavir-environment)

(defmethod cleavir-i18n:report-condition
    ((condition no-variable-info) stream (langauge cleavir-i18n:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined variable named ~s" (name condition))))

(defmethod cleavir-i18n:report-condition
    ((condition no-function-info) stream (langauge cleavir-i18n:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined function named ~s" (name condition))))

(defmethod cleavir-i18n:report-condition
    ((condition no-block-info) stream (langauge cleavir-i18n:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined block named ~s" (name condition))))

(defmethod cleavir-i18n:report-condition
    ((condition no-tag-info) stream (langauge cleavir-i18n:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined tag named ~s" (name condition))))
