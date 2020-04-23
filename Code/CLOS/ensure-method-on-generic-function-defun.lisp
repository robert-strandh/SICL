(cl:in-package #:sicl-clos)

;;; This function exists solely for reasons of bootstrapping.  The
;;; definition below is the one that is present in the final system.
;;; During bootstrapping, however, the expansion of the DEFMETHOD
;;; macro (which contains a call to this function) must call
;;; ENSURE-METHOD in the preceding environment.  Thus, during
;;; bootstrapping, this function is defined differently.
(defun ensure-method-on-generic-function (&rest arguments)
  (apply #'ensure-method arguments))
