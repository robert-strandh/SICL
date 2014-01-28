(in-package #:sicl-clos)

(defmethod add-dependent ((obj standard-class) dependent)
  (pushnew dependent (dependents obj)))

(defmethod add-dependent ((obj funcallable-standard-class) dependent)
  (pushnew dependent (dependents obj)))

(defmethod add-dependent ((obj standard-generic-function) dependent)
  (pushnew dependent (dependents obj)))

(defmethod remove-dependent ((obj standard-class) dependent)
  (setf (dependents obj)
	(remove dependent (dependents obj) :test #'eq)))

(defmethod remove-dependent ((obj funcallable-standard-class) dependent)
  (setf (dependents obj)
	(remove dependent (dependents obj) :test #'eq)))

(defmethod remove-dependent ((obj standard-generic-function) dependent)
  (setf (dependents obj)
	(remove dependent (dependents obj) :test #'eq)))

(defmethod map-dependents ((obj standard-class) function)
  (mapc function (dependents obj)))

(defmethod map-dependents ((obj funcallable-standard-class) function)
  (mapc function (dependents obj)))

(defmethod map-dependents ((obj standard-generic-function) function)
  (mapc function (dependents obj)))



