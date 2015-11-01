(cl:in-package #:sicl-clos)

(defmethod class-prototype :before ((class class))
  (unless (class-finalized-p class)
    (error "the class is not finalized")))
