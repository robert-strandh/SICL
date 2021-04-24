(cl:in-package #:sicl-data-and-control-flow)

(defmethod acclimation:report-condition
    ((condition odd-number-of-arguments-to-setf)
     stream
     (language acclimation:english))
  (format stream
          "An odd number of arguments was given to SETF~@
           in the following form:~@
           ~s"
          (form condition)))

(defmethod acclimation:report-condition
    ((condition odd-number-of-arguments-to-psetf)
     stream
     (language acclimation:english))
  (format stream
          "An odd number of arguments was given to PSETF~@
           in the following form:~@
           ~s"
          (form condition)))

(defmethod acclimation:report-condition
    ((condition odd-number-of-arguments-to-psetq)
     stream
     (language acclimation:english))
  (format stream
          "An odd number of arguments was given to PSETQ~@
           in the following form:~@
           ~s"
          (form condition)))

(defmethod acclimation:report-condition
    ((condition too-few-arguments-to-shiftf)
     stream
     (language acclimation:english))
  (format stream
          "Too few arguments were given to SHIFTF~@
           in the following form:~@
           ~s"
          (form condition)))
