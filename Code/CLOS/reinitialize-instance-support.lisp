(cl:in-package #:sicl-clos)

(defun reinitialize-instance-default
    (instance &rest initargs &key &allow-other-keys)
  ;; Call shared-initialize with a slot-list of (), meaning no slot,
  ;; i.e., only assign values to slots that have explicit
  ;; initialization arguments in initargs. 
  (apply #'shared-initialize instance () initargs))
