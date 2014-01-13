(in-package #:sicl-clos)

(defgeneric finalize-inheritance (class))

(defgeneric allocate-instance (class &rest initargs))

(defun allocate-instance-aux (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((number-of-slots (count :instance 
				(class-slots class)
				:test #'eq :key #'slot-definition-allocation)))
    (allocate-heap-instance
     class
     (make-array number-of-slots :initial-element *secret-unbound-value*))))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-aux class))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-aux class))

(defun standard-instance-access (instance location)
  (slot-contents (heap-instance-slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (slot-contents (heap-instance-slots instance) location) new-value))

