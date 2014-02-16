(cl:in-package #:sicl-clos)

;;;; This function implements the action of the default methods on
;;;; ALLOCATE-INSTANCE, i.e., the methods specialized for
;;;; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.  Every instance of
;;;; these classes has two initial cells (the unique number of the
;;;; class, and the list of effective slots of the class) in the
;;;; contents vector.  These cells are not counted among the slots,
;;;; because they are accessed directly, using offsets.  For that
;;;; reason, we must allocate more slot storage than there are slots
;;;; with :INSTANCE allocation.

(defun allocate-instance-default (class &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((slots (class-slots class))
	 (slot-count (count :instance slots
			    :test #'eq :key #'slot-definition-allocation))
	 (size (+ slot-count 2))
	 (slots (allocate-slot-storage size))
	 (instance (allocate-heap-instance class slots)))
    ;; Store the unique number of the class in the instance.
    (setf (standard-instance-access instance +class-unique-number-offset+)
	  (unique-number class))
    instance))
