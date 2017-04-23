(cl:in-package #:cleavir-escape)

;;; FIXME FIXME FIXME this message sucks!!
(defmethod acclimation:report-condition
    ((condition incorrect-dynamic-extent)
     stream
     (language acclimation:english))
  (format stream "A variable was declared DYNAMIC-EXTENT,~@
                  but has been proven to escape."))
