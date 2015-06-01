(cl:in-package #:sicl-clos)

;;; The function in this file implements the action of the default
;;; method on the generic function MAKE-BUILT-IN-INSTANCE, i.e. the
;;; one specialized to BUILT-IN-CLASS.
;;;
;;; We use the reader DEFAULT-INITARGS rather than
;;; CLASS-DEFAULT-INITARGS, because the AMOP stipulates that
;;; CLASS-DEFAULT-INITARGS must return the empty list for built-in
;;; classes.  However, our built-in classes do have default initargs.
;;; The solution to this problem is to use a different reader named
;;; DEFAULT-INITARGS.  This reader works for standard classes and
;;; funcallable standard classes as well.

(defun make-built-in-instance-default
    (class initialize-built-in-instance &rest initargs)
  (let ((defaulted-initargs initargs))
    (loop for default-initarg in (class-default-initargs class)
	  do (unless (initarg-in-list-p (car default-initarg) initargs)
	       (setf defaulted-initargs
		     (append defaulted-initargs
			     (list (first default-initarg)
				   (funcall (third default-initarg)))))))
    (let ((instance (apply #'allocate-built-in-instance class defaulted-initargs)))
      (apply initialize-built-in-instance instance defaulted-initargs)
      instance)))
