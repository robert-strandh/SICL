(cl:in-package #:sicl-boot)

(defmethod print-object ((class standard-class) stream)
  (format stream
	  "#<Ersatz standard class ~s>"
	  (sicl-clos:class-name class)))

(defmethod print-object ((class sicl-clos:built-in-class) stream)
  (format stream
	  "#<Ersatz built-in class ~s>"
	  (sicl-clos:class-name class)))
