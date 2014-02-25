(cl:in-package #:sicl-clos)

(defun finalize-inheritance* (class)
  (setf (precedence-list class)
	(compute-class-precedence-list-assuming-superclasses-finalized class))
  (setf (c-slots class) (compute-slots class))
  (setf (c-default-initargs class) (compute-default-initargs class))
  (setf (c-finalized-p class) t))
  
