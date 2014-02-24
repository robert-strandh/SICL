(cl:in-package #:sicl-clos)

(setf *standard-direct-slot-definition*
  (find-target-class 'standard-direct-slot-definition))

(setf *standard-effective-slot-definition*
  (find-target-class 'standard-effective-slot-definition))
