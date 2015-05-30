(cl:in-package #:sicl-clos)

;;; The functions ADD-DIRECT-SUBCLASS and REMOVE-DIRECT-SUBCLASS are
;;; used to update the direct subclasses of a class, so they call this
;;; function.
(defgeneric (setf class-direct-subclasses) (direct-subclassees class))
