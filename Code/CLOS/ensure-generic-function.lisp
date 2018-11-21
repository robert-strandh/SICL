(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/ensure-generic-function.html
;;;
;;; This function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_ensure.htm#ensure-generic-function

(defun ensure-generic-function
    (name
     &rest keys
     &key (environment (sicl-genv:global-environment))
     &allow-other-keys)
  (let ((generic-function
          (if (sicl-environment:fboundp name environment)
              (let ((fun (sicl-environment:fdefinition name)))
                (if (typep fun 'generic-function)
                    fun
                    (error 'type-error
                           :datum fun
                           :expected-type '(or null generic-function))))
              nil)))
    (apply #'ensure-generic-function-using-class
           generic-function name keys)))
