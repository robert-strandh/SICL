(cl:in-package #:sicl-clos)

;;; REMOVE-DIRECT-METHOD
(defgeneric remove-direct-method (specializer method))

(defmethod remove-direct-method ((specializer class) (method method))
  (setf (specializer-direct-methods specializer)
	(remove method (specializer-direct-methods specializer))))

(defmethod remove-direct-method ((specializer eql-specializer) (method method))
  (setf (specializer-direct-methods specializer)
	(remove method (specializer-direct-methods specializer))))

;;; ADD-DIRECT-METHOD
(defgeneric add-direct-method (specializer method))

(defmethod add-direct-method ((specializer class) (method method))
  (pushnew method (specializer-direct-methods specializer)))

(defmethod add-direct-method ((specializer eql-specializer) (method method))
  (pushnew method (specializer-direct-methods specializer)))

