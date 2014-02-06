(cl:in-package #:sicl-clos)

(defun standard-instance-access (instance location)
  (slot-contents (heap-instance-slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (slot-contents (heap-instance-slots instance) location) new-value))
