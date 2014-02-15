(cl:in-package #:sicl-clos)

(setf *standard-direct-slot-definition*
  (find-bridge-class 'standard-direct-slot-definition))

(setf *standard-effective-slot-definition*
  (find-bridge-class 'standard-effective-slot-definition))
