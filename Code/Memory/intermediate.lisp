(cl:in-package #:sicl-memory)

(defun allocate-object (class size)
  (let ((contents (make-array size :initial-element *unbound*)))
    (make-heap-instance :class class :contents contents))) 

(defun contents (heap-instance location)
  (aref (heap-instance-contents heap-instance) location))

(defun (setf contents) (new-value heap-instance location)
  (setf (aref (heap-instance-contents heap-instance) location) new-value))
