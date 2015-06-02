(in-package #:sicl-clos)

;;;; This function is used to allocate an instance of a built-in
;;;; class.  We can not use a method on ALLOCATE-INSTANCE for this
;;;; purpose, because the AMOP requires the existence of a method on
;;;; ALLOCATE-INSTANCE specialized for BUILT-IN-CLASS, but it also
;;;; requires that method to signal an error. 
;;;;
;;;; Every instance of a built-in class has an initial cell (the
;;;; unique number of the class in the rack.  This cell is
;;;; not counted among the slots, because it is accessed directly,
;;;; using an offset.  The existence of this cell requires us to add 1
;;;; to the size of the rack.
;;;; 
;;;; Furthermore, instances of some built-in classes have additional
;;;; storage following the slots.  In particular, this is true for the
;;;; class ARRAY and subclasses thereof.  For that reason, this
;;;; function takes a keyword argument ADDITIONAL-STORAGE (the default
;;;; value is 0) that indicates how many additional words of storage
;;;; should be allocated following the slots in the rack.

(defun allocate-built-in-instance
    (class &rest initargs &key (additional-storage 0) &allow-other-keys)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let* ((size (+ (instance-size class) additional-storage))
	 (slots (allocate-slot-storage size))
	 (instance (allocate-heap-instance class slots)))
    ;; Store the unique number of the class in the instance so that
    ;; it can be used immediately for generic function dispatch.
    (setf (standard-instance-access instance +class-unique-number-offset+)
	  (unique-number class))
    instance))
