(cl:in-package #:sicl-clos)

(defun make-built-in-instance (class &rest initargs)
  (let ((defaulted-initargs initargs))
    (loop for default-initarg in (class-default-initargs class)
	  do (unless (initarg-in-list-p (car default-initarg) initargs)
	       (setf defaulted-initargs
		     (append defaulted-initargs
			     (list (first default-initarg)
				   (funcall (third default-initarg)))))))
    (let ((instance (apply #'allocate-built-in-instance class defaulted-initargs)))
      (apply #'initialize-built-in-instance instance defaulted-initargs)
      instance)))

