(cl:in-package #:sicl-clos)

(defun allocate-instance-default (class &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((slots (class-slots class))
	 (instance-size (count :instance slots
			       :key #'slot-definition-allocation))
	 (slots (allocate-slot-storage instance-size)))
    (allocate-heap-instance class slots)))
