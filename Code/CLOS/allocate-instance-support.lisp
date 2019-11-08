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

;;; The AMOP says that ALLOCATE-INSTANCE checks whether the class is
;;; finalized, and if not, calls FINALIZE-INHERITANCE.  However, the
;;; INITARGS received by ALLOCATE-INSTANCE should be the defaulted
;;; initargs, and computing the defaulted initargs requires the class
;;; to be finalized.  I peek at PCL shows that the class is finalized
;;; in MAKE-INSTANCE, before ALLOCATE-INSTANCE is called, which makes
;;; more sense.

(defun allocate-instance-common (class additional-size)
  (let* ((size (+ (instance-size class) additional-size))
         (instance (allocate-general-instance class size)))
    ;; Store the unique number of the class in the instance.
    (setf (general-instance-access instance +class-unique-number-offset+)
          (unique-number class))
    instance))

;;; This function implements the action of the method on
;;; ALLOCATE-INSTANCE, specialized to REGULAR-CLASS.  Every instance
;;; of a regular class has two initial cells (the STAMP, and the list
;;; of effective slots of the class) in the rack.  These cells are not
;;; counted among the slots, because they are accessed directly, using
;;; offsets.  For that reason, we must allocate more slot storage than
;;; there are slots with :INSTANCE allocation.
(defun allocate-instance-regular-class (class &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-common class 2))

;;; This function implements the action of method on
;;; ALLOCATE-INSTANCE, specialized to BUILT-IN-CLASS.  Every instance
;;; of a built-in class has one initial cell (the STAMP) in the rack.
;;; This cell is not counted among the slots, because it is accessed
;;; directly, using offsets.  For that reason, we must allocate more
;;; slot storage than there are slots with :INSTANCE allocation.
(defun allocate-instance-built-in-class (class &rest initargs)
  (declare (ignore initargs))
  (allocate-instance-common class 2))
