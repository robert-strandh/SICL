(in-package #:sicl-clos)

(defgeneric make-instance (class &rest initargs))

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defun inintarg-valid-for-some-slot-p (initarg class)
  (loop for slot in (class-slots class)
	when (member initarg (slot-definition-initargs slot) :test #'eq)
	  return t))

;;; FIXME: check validity also for generic functions

(defun initarg-in-list-p (intarg list)
  (loop for indicator in list by #'cddr
	when (eq initarg indicator)
	  return t))

(defun make-instance-aux (class &rest initargs)
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

(defmethod make-instance ((class standard-class) &rest initargs)
  (apply #'make-instance-aux class initargs))

(defmethod make-instance ((class funcallable-standard-class) &rest initargs)
  (apply #'make-instance-aux class initargs))