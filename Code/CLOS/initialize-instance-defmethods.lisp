(cl:in-package #:sicl-clos)

;; In each of these methods, we call shared-initialize with a
;; slot-list of t, meaning all slots, i.e., for every slot that is not
;; explicitly initialized and which is unbound, evaluate its initform
;; if it has one.

(defmethod initialize-instance
    ((instance standard-object) &rest initargs &key &allow-other-keys)
  (apply #'shared-initialize instance t initargs))

(defmethod initialize-instance
    ((instance function) &rest initargs &key &allow-other-keys)
  (apply #'shared-initialize instance t initargs))
