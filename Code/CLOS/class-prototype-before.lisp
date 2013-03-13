(in-package #:sicl-clos-test)

(defmethod class-prototype :before ((class class))
  (unless (finalized-p class)
    (error "the class is not finalized")))
