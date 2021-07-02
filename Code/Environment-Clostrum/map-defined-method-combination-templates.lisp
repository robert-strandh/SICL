(cl:in-package #:sicl-environment)

;;; This code should be in Clostrum in some form.  Not sure exactly
;;; which form.

(defgeneric map-defined-method-combination-templates
    (client environment function))

(defmethod map-defined-method-combination-templates
    (client (environment base-run-time-environment) function)
  (maphash function (method-combination-templates environment)))
