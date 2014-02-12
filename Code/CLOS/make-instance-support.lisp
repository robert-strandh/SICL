(cl:in-package #:sicl-clos)

;;; FIXME: check validity also for generic functions

(defun initarg-in-list-p (initarg list)
  (loop for indicator in list by #'cddr
	when (eq initarg indicator)
	  return t))

(defun make-instance-default (class &rest initargs)
  ;; FIXME: check shape of initargs (proper, length is even, etc.).
  (let ((defaulted-initargs initargs))
    (loop for default-initarg in (class-default-initargs class)
	  do (unless (initarg-in-list-p (car default-initarg) initargs)
	       (setf defaulted-initargs
		     (append defaulted-initargs
			     (list (first default-initarg)
				   (funcall (third default-initarg)))))))
    (let ((instance (apply #'allocate-instance class defaulted-initargs)))
      (apply #'initialize-instance instance defaulted-initargs)
      instance)))
