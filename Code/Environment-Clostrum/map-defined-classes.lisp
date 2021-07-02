(cl:in-package #:sicl-environment)

;;; This code should be in Clostrum in some form.  Not sure exactly
;;; which form.

(defgeneric map-defined-classes (client environment function))

(defmethod map-defined-classes
    (client (environment clostrum/virtual:virtual-run-time-environment) function)
  (maphash function (clostrum/virtual::classes environment)))
