(cl:in-package #:sicl-clos)

(defun add-dependent-default (metaobject dependent)
  (pushnew dependent (dependents metaobject) :test #'eq))

(defun remove-dependent-default (metaobject dependent)
  (setf (dependents obj)
	(remove dependent (dependents obj) :test #'eq)))

(defun map-dependents-default (metaobject function)
  (mapc function (dependents metaobject)))
