(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ADD-DIRECT-METHOD and REMOVE-DIRECT-METHOD
;;; ADD-METHOD and REMOVE-METHOD

;;; REMOVE-DIRECT-METHOD
(defgeneric remove-direct-method (specializer method))

(defmethod remove-direct-method ((specializer class) (method method))
  (setf (specializer-direct-methods specializer)
	(remove method (specializer-direct-methods specializer))))

(defmethod remove-direct-method ((specializer eql-specializer) (method method))
  (setf (specializer-direct-methods specializer)
	(remove method (specializer-direct-methods specializer))))

;;; ADD-DIRECT-METHOD
(defgeneric add-direct-method (specializer method))

(defmethod add-direct-method ((specializer class) (method method))
  (pushnew method (specializer-direct-methods specializer)))

(defmethod add-direct-method ((specializer eql-specializer) (method method))
  (pushnew method (specializer-direct-methods specializer)))

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
   (compute-discriminating-function generic-function))
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
   (compute-discriminating-function generic-function))
  (map-dependents generic-function
		  (lambda (dependent)
		    (update-dependent generic-function
				      dependent
				      'add-method
				      method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-METHOD and DEFMETHOD.

(defun parse-defmethod (args)
  (let ((name (pop args))
	(qualifiers (loop while (and (consp args) (not (listp (car args))))
			  collect (pop args)))
	(lambda-list (pop args)))
    ;; FIXME: handle declarations and documentation
    ;; FIXME: check the lambda list.  proper list, etc. 
    (let ((pos (position-if (lambda (x)
			      (member x '(&optional &rest &body &key
					  &allow-other-keys &aux)))
			    lambda-list)))
      (when (null pos)
	(setf pos (length lambda-list)))
      (values name
	      qualifiers
	      (append (mapcar (lambda (parameter)
				(if (consp parameter)
				    (car parameter)
				    parameter))
			      (subseq lambda-list 0 pos))
		      (subseq lambda-list pos))
	      (mapcar (lambda (parameter)
			(if (consp parameter)
			    (cadr parameter)
			    t))
		      (subseq lambda-list 0 pos))
	      args))))

(defun ensure-method (generic-function &rest keys)
  (let ((method (apply #'make-instance
		       'standard-method
		       keys)))
    (add-method generic-function method)
    method))

(defun canonicalize-specializers (specializers)
  ;; FIXME: handle eql specializers.
  (mapcar #'find-class specializers))

(defmacro defmethod (&rest arguments)
  (multiple-value-bind (name qualifiers lambda-list specializers body)
      (parse-defmethod arguments)
    (let ((generic-function-var (gensym)))
    ;; FIXME: do the lambda list
      `(let ((,generic-function-var (ensure-generic-function ',name)))
	 (ensure-method
	  ,generic-function-var
	  :lambda-list ',lambda-list
	  :qualifiers ',qualifiers
	  :specializers ',(canonicalize-specializers specializers)
	  :body ',body
	  :function (make-method-lambda
		     ,generic-function-var
		     (class-prototype
		      (generic-function-method-class ,generic-function-var))
		     '(lambda ,lambda-list ,@body)
		     nil))))))

