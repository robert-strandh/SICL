(cl:in-package #:sicl-clos)

;;; The AMOP says that ALLOCATE-INSTANCE checks whether the class is
;;; finalized, and if not, calls FINALIZE-INHERITANCE.  However, the
;;; INITARGS received by ALLOCATE-INSTANCE should be the defaulted
;;; initargs, and computing the defaulted initargs requires the class
;;; to be finalized.  A peek at PCL shows that the class is finalized
;;; in MAKE-INSTANCE, before ALLOCATE-INSTANCE is called, which makes
;;; more sense.

;;; FIXME: check validity also for generic functions

(defun make-instance-default (class initialize-instance &rest initargs)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  ;; FIXME: check shape of initargs (proper, length is even, etc.).
  (let ((defaulted-initargs initargs))
    (loop with default = (load-time-value (list nil))
          for (name form thunk) in (class-default-initargs class)
          do (when (eq (getf initargs name default) default)
               (setf defaulted-initargs
                     (append defaulted-initargs
                             (list name (funcall thunk))))))
    (let ((instance (apply #'allocate-instance class defaulted-initargs)))
      (apply initialize-instance instance defaulted-initargs)
      instance)))
