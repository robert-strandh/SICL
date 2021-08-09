(cl:in-package #:cleavir-code-utilities)

(defmethod acclimation:report-condition
    ((c ordinary-body-must-be-proper-list) stream (language acclimation:english))
  (format stream
          "The BODY of a form such as LET, LET*, FLET, LABELS,~@
           or LOCALLY, must be a proper list.
           But the following was found instead:~@
           ~s"
          (body c)))

(defmethod acclimation:report-condition
    ((c function-body-must-be-proper-list) stream (language acclimation:english))
  (format stream
          "The BODY of a function must be a proper list.
           But the following was found instead:~@
           ~s"
          (body c)))
