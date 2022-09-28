(cl:in-package #:sicl-data-and-control-flow)

(defmethod acclimation:report-condition
    ((condition too-few-arguments-to-shiftf)
     stream
     (language acclimation:english))
  (format stream
          "Too few arguments were given to SHIFTF~@
           in the following form:~@
           ~s"
          (form condition)))
