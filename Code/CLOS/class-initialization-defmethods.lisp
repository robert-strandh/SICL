(cl:in-package #:sicl-clos)

(defmethod initialize-instance :after
    ((class regular-class) &rest initargs &key &allow-other-keys)
  (apply #'initialize-instance-after-regular-class-default
	 class initargs))

(defmethod shared-initialize :around
    ((class real-class)
     slot-names
     &rest initargs
     &key
       direct-default-initargs
       direct-superclasses
       direct-slots
     &allow-other-keys)
  (check-direct-default-initargs direct-default-initargs)
  (check-direct-superclasses class direct-superclasses)
  (when (null direct-superclasses)
    (setf direct-superclasses (default-superclasses class)))
  (apply #'call-next-method
	 class
	 slot-names
	 :direct-superclasses
	 (default-superclasses class)
	 :direct-default-initargs direct-default-initargs
	 :direct-slots (check-and-instantiate-direct-slots class direct-slots)
	 initargs))

;;; According to the AMOP, calling initialize-instance on a built-in
;;; class (i.e., on an instance of a subclass of the class
;;; BUILT-IN-CLASS) signals an error, because built-in classes can not
;;; be created by the user.  But during the bootstrapping phase we
;;; need to create built-in classes.  We solve this problem by
;;; removing this method once the bootstrapping phase is finished.
;;;
;;; We do not add readers and writers here, because we do it in
;;; ENSURE-BUILT-IN-CLASS after we have finalized inheritance.  The
;;; reason for that is that we then know the slot location.
(defmethod initialize-instance :after
    ((class built-in-class) &rest initargs &key &allow-other-keys)
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
