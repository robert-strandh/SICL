(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-DEPENDENT.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-dependent.html
(defgeneric add-dependent (metaobject dependent))

(defun add-dependent-default (metaobject dependent)
  (pushnew dependent (dependents metaobject) :test #'eq))

(defmethod add-dependent ((metaobject standard-class) dependent)
  (add-dependent-default metaobject dependent))

(defmethod add-dependent ((metaobject funcallable-standard-class) dependent)
  (add-dependent-default metaobject dependent))

(defmethod add-dependent ((metaobject standard-generic-function) dependent)
  (add-dependent-default metaobject dependent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REMOVE-DEPENDENT.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-dependent.html
(defgeneric remove-dependent (metaobject dependent))

(defun remove-dependent-default (metaobject dependent)
  (setf (dependents metaobject)
        (remove dependent (dependents metaobject) :test #'eq)))

(defmethod remove-dependent ((metaobject standard-class) dependent)
  (remove-dependent-default metaobject dependent))

(defmethod remove-dependent ((metaobject funcallable-standard-class) dependent)
  (remove-dependent-default metaobject dependent))

(defmethod remove-dependent ((metaobject standard-generic-function) dependent)
  (remove-dependent-default metaobject dependent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAP-DEPENDENTS.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/map-dependents.html
(defgeneric map-dependents (metaobject function))

(defun map-dependents-default (metaobject function)
  (mapc function (dependents metaobject)))

(defmethod map-dependents ((metaobject standard-class) function)
  (map-dependents-default metaobject function))

(defmethod map-dependents ((metaobject funcallable-standard-class) function)
  (map-dependents-default metaobject function))

(defmethod map-dependents ((metaobject standard-generic-function) function)
  (map-dependents-default metaobject function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function UPDATE-DEPENDENT.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/update-dependent.html
(defgeneric update-dependent (metaobject dependent &rest initargs))
