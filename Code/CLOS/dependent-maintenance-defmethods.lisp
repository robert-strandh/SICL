(cl:in-package #:sicl-clos)

(defmethod add-dependent ((metaobject standard-class) dependent)
  (add-dependent-default metaobject dependent))

(defmethod add-dependent ((metaobject funcallable-standard-class) dependent)
  (add-dependent-default metaobject dependent))

(defmethod add-dependent ((metaobject standard-generic-function) dependent)
  (add-dependent-default metaobject dependent))

(defmethod remove-dependent ((metaobject standard-class) dependent)
  (remove-dependent-default metaobject dependent))

(defmethod remove-dependent ((metaobject funcallable-standard-class) dependent)
  (remove-dependent-default metaobject dependent))

(defmethod remove-dependent ((metaobject standard-generic-function) dependent)
  (remove-dependent-default metaobject dependent))

(defmethod map-dependents ((metaobject standard-class) function)
  (map-dependents-default metaobject function))

(defmethod map-dependents ((metaobject funcallable-standard-class) function)
  (map-dependents-default metaobject function))

(defmethod map-dependents ((metaobject standard-generic-function) function)
  (map-dependents-default metaobject function))
