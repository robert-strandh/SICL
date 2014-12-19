(cl:in-package #:sicl-clos)

(defmethod class-default-initargs ((class forward-referenced-class))
  (declare (ignore class))
  (error "A forward referenced class does not have any default initargs"))

(defmethod class-precedence-list ((class forward-referenced-class))
  (declare (ignore class))
  (error "A forward referenced class does not have a precedence list"))

(defmethod class-slots ((class forward-referenced-class))
  (declare (ignore class))
  (error "A forward referenced class does not have any slots"))
