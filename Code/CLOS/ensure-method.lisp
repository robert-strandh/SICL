(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD.
;;;
;;; This function is not required by the AMOP to exist, 

(defun ensure-method (generic-function &rest keys)
  (let ((method (apply #'make-instance
		       'standard-method
		       keys)))
    (add-method generic-function method)
    method))
