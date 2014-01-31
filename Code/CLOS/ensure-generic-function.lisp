(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function.html
;;;
;;; This function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_ensure.htm#ensure-generic-function

(defun ensure-generic-function (name &rest keys)
  ;; FIXME: check if it names a non-generic function or macro already. 
  (let ((generic-function (find-generic-function name)))
    (apply #'ensure-generic-function-using-class
	   generic-function name keys)))
