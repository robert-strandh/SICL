(cl:in-package #:sicl-clos)

(defun add-dependent (metaobject dependent)
  (add-dependent-default metaobject dependent))

(defun remove-dependent (metaobject dependent)
  (remove-dependent-default metaobject dependent))

(defun map-dependents (metaobject function)
  (map-dependents-default metaobject function))

(defun update-dependent (metaobject dependent &rest initargs)
  (declare (ignore metaobject dependent initargs))
  nil)
