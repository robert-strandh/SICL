(cl:in-package #:sicl-new-boot-phase-2)

;;; The arguments received by ENSURE-GENERIC-FUNCTION are not quite as
;;; documented, because we can make certain shortcuts here, given that
;;; this function is going to be called only from the expansion of
;;; DEFGENERIC, and we have full control over that expansion.
(defun define-ensure-generic-function (client e1 e2)
  (setf (clo:fdefinition client e2 'ensure-generic-function)
        (lambda (name
                 &key
                   argument-precedence-order
                   declare
                   documentation
                   environment
                   generic-function-class
                   lambda-list
                   method-class
                   method-combination)
          (declare (ignore declare documentation environment))
          (assert (null (clo:fboundp client e2 name)))
          (setf (clo:fdefinition client e2 name)
                (make-instance
                    (clo:find-class client e1 generic-function-class t)
                  :argument-precedence-order argument-precedence-order
                  :lambda-list lambda-list
                  :method-class (clo:find-class client e1 method-class t)
                  :method-combination method-combination)))))
