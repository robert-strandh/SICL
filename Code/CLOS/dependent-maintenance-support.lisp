(cl:in-package #:sicl-clos)

(defun add-dependent-default (metaobject dependent)
  (pushnew dependent (dependents metaobject) :test #'eq))

(defun remove-dependent-default (metaobject dependent)
  (setf (dependents metaobject)
        (remove dependent (dependents metaobject) :test #'eq)))

(defun map-dependents-default (metaobject function)
  (mapc function (dependents metaobject)))
