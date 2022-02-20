(cl:in-package #:sicl-environment)

;;; This code should be in Clostrum in some form.  Not sure exactly
;;; which form.

(defgeneric map-defined-functions (client environment function))

(defmethod map-defined-functions
    (client (environment clostrum-basic:run-time-environment) function)
  (loop with function-entries = (clostrum-basic::functions environment)
        for name being each hash-key of function-entries
        do (when (and (fboundp client environment name)
                      (or (consp name)
                          (and (null (macro-function client environment name))
                               (not (special-operator client environment name)))))
             (funcall function name (fdefinition client environment name)))))
