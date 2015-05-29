(cl:in-package #:sicl-clos)

(defun default-superclasses (class)
  (cond ((eq (class-of class) (find-class 'standard-class))
	 (list (find-class 'standard-object)))
	((eq (class-of class) (find-class 'funcallable-standard-class))
	 (list (find-class 'funcallable-standard-object)))
	(t
	 '())))
