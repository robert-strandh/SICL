(cl:in-package #:sicl-boot-phase-7)

(defun patch-direct-slot-definition (slot-definition e5)
  (setf (slot-value slot-definition 'sicl-boot::%class)
        (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e5)))

(defun patch-effective-slot-definition (slot-definition e5)
  (setf (slot-value slot-definition 'sicl-boot::%class)
        (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition e5)))
