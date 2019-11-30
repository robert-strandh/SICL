(cl:in-package #:sicl-clos)

;;; The AMOP says that ALLOCATE-INSTANCE checks whether the class is
;;; finalized, and if not, calls FINALIZE-INHERITANCE.  However, the
;;; INITARGS received by ALLOCATE-INSTANCE should be the defaulted
;;; initargs, and computing the defaulted initargs requires the class
;;; to be finalized.  A peek at PCL shows that the class is finalized
;;; in MAKE-INSTANCE, before ALLOCATE-INSTANCE is called, which makes
;;; more sense.

;;; FIXME: check validity also for generic functions

(defun initarg-in-list-p (initarg list)
  (loop for indicator in list by #'cddr
        when (eq initarg indicator)
          return t))

(defun make-instance-default (class initialize-instance &rest initargs)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  ;; FIXME: check shape of initargs (proper, length is even, etc.).
  (let ((defaulted-initargs initargs))
    (loop for default-initarg in (class-default-initargs class)
          do (unless (initarg-in-list-p (car default-initarg) initargs)
               (setf defaulted-initargs
                     (append defaulted-initargs
                             (list (first default-initarg)
                                   (funcall (third default-initarg)))))))
    (let ((instance (apply #'allocate-instance class defaulted-initargs)))
      (apply initialize-instance instance defaulted-initargs)
      instance)))
