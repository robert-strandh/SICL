(cl:in-package #:sicl-clos)

;;; The method defined here is meant to implement the behavior
;;; described in "Initialization of Method Metaobjects" in the AMOP.

;;; FIXME: Here, I specialize on METHOD, but check whether the
;;; specialization should be reserved for STANDARD-METHOD instead.

(defmethod initialize-instance :after ((method method)
				       &rest keys
				       &key
					 qualifiers
					 lambda-list
					 specializers
					 function
					 documentation)
  (declare (ignore qualifiers lambda-list specializers function documentation))
  (apply #'initialize-instance-after-method method keys))
				       
(defmethod initialize-instance :after ((method standard-accessor-method)
				       &rest keys
				       &key
					 slot-definition
				       &allow-other-keys)
  (declare (ignore slot-definition))
  (apply #'initialize-instance-after-standard-accessor-method method keys))
