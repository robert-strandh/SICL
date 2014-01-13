(cl:in-package #:sicl-clos)

;;; REMOVE-METHOD
(defgeneric remove-method (generic-function method))

(defmethod remove-method ((generic-function standard-generic-function)
			  (method standard-method))
  (setf (gf-methods generic-function)
	(remove method (generic-function-methods generic-function)))
  ;; The method is no longer associated with this generic funtion.
  (setf (method-generic-function method) nil)
  ;; Remove this method from the set of direct methods of its
  ;; specializers.
  (loop for specializer in (method-specializers method)
	do (remove-direct-method specializer method))
  ;; The specializer profile of the generic function must be updated.
  (setf (specializer-profile generic-function)
	(compute-specializer-profile
	 (generic-function-methods generic-function)))
  ;; Trash the call history, because it could be all wrong now. 
  (setf (call-history generic-function) '())
  (set-funcallable-instance-function
   (compute-discriminating-function generic-function)
   generic-function)
  (map-dependents generic-function
		  (lambda (dependent)
		    (update-dependent generic-function
				      dependent
				      'remove-method
				      method))))

;;; ADD-METHOD
(defgeneric add-method (generic-function method))

(defmethod add-method ((generic-function standard-generic-function)
		       (method standard-method))
  (unless (null (method-generic-function method))
    (error "method is already associated with a generic-function ~s"
	   method))
  ;; FIXME: check that the lambda list are congruent
  ;; See if there is a method with the same specializers and the
  ;; same qualifiers.
  (let ((method-to-remove
	  (find-if (lambda (existing-method)
		     ;; They must have the same qualifiers
		     ;; and the same specializers.
		     (and (null (set-exclusive-or
				 (method-qualifiers method)
				 (method-qualifiers existing-method)))
			  (equal (method-specializers method)
				 (method-specializers existing-method))))
		   (generic-function-methods generic-function))))
    (unless (null method-to-remove)
      (remove-method generic-function method-to-remove)))
  ;; Associate the method with this generic function.
  (setf (method-generic-function method) generic-function)
  ;; Add this method to the set of direct methods of its
  ;; specializers.
  (loop for specializer in (method-specializers method)
	do (add-direct-method specializer method))
  ;; Add this method to the set of methods of this generic function.
  (setf (gf-methods generic-function)
	(cons method (generic-function-methods generic-function)))
  ;; The specializer profile of the generic function must be updated.
  (setf (specializer-profile generic-function)
	(compute-specializer-profile
	 (generic-function-methods generic-function)))
  ;; Trash the call history, because it could be all wrong now. 
  (setf (call-history generic-function) '())
  (set-funcallable-instance-function
   (compute-discriminating-function generic-function)
   generic-function)
  (map-dependents generic-function
		  (lambda (dependent)
		    (update-dependent generic-function
				      dependent
				      'add-method
				      method))))
