(cl:in-package #:sicl-clos)

;;; We use the reader DEFAULT-INITARGS rather than
;;; CLASS-DEFAULT-INITARGS, because the AMOP stipulates that
;;; CLASS-DEFAULT-INITARGS must return the empty list for built-in
;;; classes.  However, our built-in classes do have default initargs.
;;; The solution to this problem is to use a different reader named
;;; DEFAULT-INITARGS.  This reader works for standard classes and
;;; funcallable standard classes as well.

(defun make-built-in-instance (class &rest initargs)
  (let ((defaulted-initargs initargs))
    (loop for default-initarg in (default-initargs class)
	  do (unless (initarg-in-list-p (car default-initarg) initargs)
	       (setf defaulted-initargs
		     (append defaulted-initargs
			     (list (first default-initarg)
				   (funcall (third default-initarg)))))))
    (let ((instance (apply #'allocate-built-in-instance class defaulted-initargs)))
      (apply #'initialize-built-in-instance instance defaulted-initargs)
      instance)))
