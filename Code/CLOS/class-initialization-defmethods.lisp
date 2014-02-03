(cl:in-package #:sicl-clos)

(defmethod initialize-instance :after
    ((class standard-class)
     &rest initargs
     &key
       direct-default-initargs
       direct-superclasses
       direct-slots
       &allow-other-keys)
  (declare (ignore direct-default-initargs
		   direct-superclasses
		   direct-slots))
  (apply #'initialize-instance-after-standard-class-default
	 class initargs))

(defmethod initialize-instance :after
    ((class funcallable-standard-class)
     &rest initargs
     &key
       direct-default-initargs
       direct-superclasses
       direct-slots
       &allow-other-keys)
  (declare (ignore direct-default-initargs
		   direct-superclasses
		   direct-slots))
  (apply #'initialize-instance-after-funcallable-standard-class-default
	 class initargs))

(defmethod initialize-instance :after
    ((class built-in-class)
     &rest initargs
     &key
       direct-default-initargs
       direct-superclasses
       direct-slots
       &allow-other-keys)
  (declare (ignore direct-default-initargs
		   direct-superclasses
		   direct-slots))
  (apply #'initialize-instance-after-built-in-class-default
	 class initargs))

;;; I don't know why this definition makes SBCL go into an infinite
;;; recursion.
;; (defmethod reinitialize-instance :after
;;     ((class standard-class)
;;      &rest args
;;      &key
;;      &allow-other-keys)
;;   (map-dependents class
;; 		  (lambda (dependent)
;; 		    (apply #'update-dependent class dependent args))))
