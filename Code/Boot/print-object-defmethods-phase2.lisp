(cl:in-package #:sicl-boot)

(defmethod print-object ((class standard-class) stream)
  (format stream
	  "#<Ersatz standard class ~s>"
	  (sicl-clos:class-name class)))
