(cl:in-package #:sicl-clos)

(defmethod initialize-instance ((instance standard-object)
				&rest initargs
				&key &allow-other-keys)
  (apply #'initialize-instance-default instance initargs))

(defmethod reinitialize-instance ((instance standard-object)
				  &rest initargs
				  &key &allow-other-keys)
  (apply #'reinitialize-instance-default instance initargs))

(defmethod shared-initialize ((instance standard-object)
			      slot-names
			      &rest initargs)
  (apply #'shared-initialize-default instance slot-names initargs))
