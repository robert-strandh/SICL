(in-package #:sicl-clos)

(defun built-in-find-slot (object slot-name)
  (let* ((class (class-of object))
	 (slots (built-in-class-slots class))
	 (name-test (lambda (slot-definition)
		      (if (eq (class-of slot-definition)
			      *standard-effective-slot-definition*)
			  (standard-instance-access
			   object
			   *standard-effective-slot-definition-name-location*)
			  (slot-definition-name slot-definition)))))
    (find slot-name slots :test #'eq :key name-test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILT-in-SLOT-VALUE, (SETF BUILT-IN-SLOT-VALUE), 
;;; BUILT-IN-SLOT-VALUE-USING-CLASS (SETF BUILT-IN-SLOT-VALUE-USING-CLASS)

(defmethod built-in-slot-value-using-class ((class built-in-class)
				   object
				   (slot standard-effective-slot-definition))
  (slot-value-using-class-aux class object slot))

(defmethod (setf built-in-slot-value-using-class)
  (new-value
   (class built-in-class)
   object
   (slot standard-effective-slot-definition))
  (setf (slot-value-using-class-aux object slot) new-value))

(defun built-in-slot-value (object slot-name)
  (unless (heap-instance-p object)
    (error "object must be a heap instance"))
  (let ((class (class-of object)))
    ;; The first element of the contents vector is the list of
    ;; effective slots of the class of the object.
    (let* ((slots (slot-contents (heap-instance-slots object) 0))
	   (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
      (if (null slot)
	  (slot-missing class object slot-name 'built-in-slot-value)
	  (built-in-slot-value-using-class class object slot)))))

(defun (setf built-in-slot-value) (new-value object slot-name)
  (unless (heap-instance-p object)
    (error "object must be a heap instance"))
  (let ((class (class-of object)))
    ;; The first element of the contents vector is the list of
    ;; effective slots of the class of the object.
    (let* ((slots (slot-contents (heap-instance-slots object) 0))
	   (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
      (if (null slot)
	  (slot-missing class object slot-name 'built-in-slot-value)
	  (setf (built-in-slot-value-using-class class object slot) new-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILT-IN-SLOT-BOUNDP BUILT-IN-SLOT-BOUNDP-USING-CLASS

(defmethod built-in-slot-boundp-using-class ((class built-in-class)
				    object
				    (slot standard-effective-slot-definition))
  (built-in-slot-boundp-using-class-aux object slot))

(defun built-in-slot-boundp (object slot-name)
  (unless (heap-instance-p object)
    (error "object must be a heap instance"))
  (let ((class (class-of object)))
    ;; FIXME: check that the object is up to date.  
    ;; 
    ;; The first element of the contents vector is the list of
    ;; effective slots of the class of the object.
    (let* ((slots (slot-contents (heap-instance-slots object) 0))
	   (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
      (if (null slot)
	  (slot-missing class object slot-name 'slot-makunbound)
	  (built-in-slot-boundp-using-class class object slot)))))

