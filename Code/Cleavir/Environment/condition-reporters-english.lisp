(cl:in-package #:cleavir-environment)

(defmethod acclimation:report-condition
    ((condition no-variable-info) stream (language acclimation:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined variable named ~s" (name condition))))

(defmethod acclimation:report-condition
    ((condition no-function-info) stream (language acclimation:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined function named ~s" (name condition))))

(defmethod acclimation:report-condition
    ((condition no-block-info) stream (language acclimation:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined block named ~s" (name condition))))

(defmethod acclimation:report-condition
    ((condition no-tag-info) stream (language acclimation:english))
  (let ((*package* (find-package '#:keyword)))
    (format stream "Undefined tag named ~s" (name condition))))
