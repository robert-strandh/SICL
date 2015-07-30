(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ALLOCATE-INSTANCE.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/allocate-instance.html
;;;
;;; The specification includes three methods for this generic
;;; function.  The first one is specialized for STANDARD-CLASS.  The
;;; second one is specialized for FUNCALLABLE-STANDARD-CLASS.  The
;;; third one is specialized for BUILT-IN-CLASS.  The last one signals
;;; an error.

;;; This function implements the action of the default methods on
;;; ALLOCATE-INSTANCE, i.e., the methods specialized for
;;; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.  Every instance of
;;; these classes has two initial cells (the STAMP, and the list of
;;; effective slots of the class) in the rack.  These cells are not
;;; counted among the slots, because they are accessed directly, using
;;; offsets.  For that reason, we must allocate more slot storage than
;;; there are slots with :INSTANCE allocation.

;;; The AMOP says that ALLOCATE-INSTANCE checks whether the class is
;;; finalized, and if not, calls FINALIZE-INHERITANCE.  However, the
;;; INITARGS received by ALLOCATE-INSTANCE should be the defaulted
;;; initargs, and computing the defaulted initargs requires the class
;;; to be finalized.  I peek at PCL shows that the class is finalized
;;; in MAKE-INSTANCE, before ALLOCATE-INSTANCE is called, which makes
;;; more sense.

(defun allocate-instance-default (class &rest initargs)
  (declare (ignore initargs))
  (let* ((slots (allocate-slot-storage (instance-size class)))
	 (instance (allocate-heap-instance class slots)))
    ;; Store the unique number of the class in the instance.
    (setf (standard-instance-access instance +class-unique-number-offset+)
	  (unique-number class))
    instance))
