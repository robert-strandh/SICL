(cl:in-package #:sicl-clos)

;;; For the specification of this function, see
;;; http://metamodular.com/CLOS-MOP/ensure-class.html
(defun ensure-class (name &rest arguments &key &allow-other-keys)
  (unless (and (symbolp name) (not (null name)))
    (error 'class-name-must-be-non-nil-symbol
           :name 'ensure-class
           :datum name))
  (apply #'ensure-class-using-class
         (find-class name nil)
         name
         arguments))
