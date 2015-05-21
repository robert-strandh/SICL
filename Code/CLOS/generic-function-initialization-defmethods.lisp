(cl:in-package #:sicl-clos)

;;; FIXME: check whether these methods should specialize on
;;; GENERIC-FUNCTION rather than on STANDARD-GENERIC-FUNCTION.

(defmethod initialize-instance :after
    ((generic-function standard-generic-function)
     &rest initargs
     &key
     &allow-other-keys)
  (apply #'initialize-instance-after-standard-generic-function-default
	 generic-function initargs))

(defmethod shared-initialize :before
    ((generic-function generic-function)
     slot-names
     &rest initargs
     &key
     &allow-other-keys)
  (apply #'shared-initialize-before-generic-function
	 generic-function
	 slot-names
	 initargs))

(defmethod shared-initialize :after
    ((generic-function generic-function)
     slot-names
     &rest initargs
     &key
     &allow-other-keys)
  (apply #'shared-initialize-after-generic-function
	 generic-function
	 slot-names
	 initargs))

(defmethod reinitialize-instance :after
    ((generic-function standard-generic-function)
     &rest initargs
     &key
     &allow-other-keys)
  (apply #'reinitialize-instance-after-standard-generic-function-default
	 generic-function initargs))
