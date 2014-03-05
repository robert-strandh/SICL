(cl:in-package #:sicl-clos)

(defun initialize-instance-default
    (instance &rest initargs &key &allow-other-keys)
  ;; Call shared-initialize with a slot-list of t, meaning all slots,
  ;; i.e., for every slot that is not explicitly initialized and which
  ;; is unbound, evaluate its initform if it has one. 
  (apply #'shared-initialize instance t initargs))
