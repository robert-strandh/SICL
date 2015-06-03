(cl:in-package #:sicl-clos)

(defclass function (t)
  ((%entry-point :initarg :entry-point)
   (%linkage-rack :initarg :linkage-rack)
   (%environment :initform nil :initarg :environment))
  (:metaclass built-in-class))
