(in-package #:sicl-clos)

(defun allocate-built-in-instance
    (class &rest initargs &key (additional-storage 0))
  (declare (ignore initargs))
  (let* ((number-of-slots (count :instance 
				 (class-slots class)
				 :test #'eq :key #'slot-definition-allocation))
	 (size (+ number-of-slots additional-storage)))
    (allocate-heap-instance
     class
     (make-array size :initial-element *secret-unbound-value*))))
