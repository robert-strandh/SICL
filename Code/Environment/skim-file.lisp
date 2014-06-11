(cl:in-package #:sicl-global-environment)

;;; Skim a compound form with respect to ENVIRONMENT, where HEAD is
;;; the CAR of the form, FORM is the entire form. 
(defgeneric skim-compound-form (head form environment))

;;; Skim FORM with respect to ENVIRONMENT.
(defun skim-form (form environment)
  (let ((expanded-form (fully-expand-form form environment)))
    (when (consp expanded-form)
      (skim-compound-form (car expanded-form) expanded-form environment))))

(defmethod skim-compound-form (head form environment)
  (declare (ignore head form environment))
  nil)
