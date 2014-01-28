(in-package #:sicl-clos)

(defmethod shared-initialize ((instance standard-object)
			      slot-names
			      &rest initargs)
  (let ((class-slots (class-slots (class-of instance))))
    (loop for slot in class-slots
	  for slot-name = (slot-definition-name slot)
	  do (multiple-value-bind (key value foundp)
		 ;; Find the first key/value pair in initargs where the
		 ;; key is one of the initargs of the slot. 
		 (get-properties initargs (slot-definition-initargs slot))
	       (declare (ignore key))
	       (if foundp
		   ;; Found an explicit initarg in initargs.  Initialize
		   ;; the slot from its value
		   (setf (slot-value instance slot-name) value)
		   ;; No explicit initarg found.  
		   (when (and (not (slot-boundp instance slot-name))
			      (not (null (slot-definition-initfunction slot)))
			      (or (eq slot-names t)
				  (member slot-name slot-names)))
		     ;; Evaluate the initform by executing the initfunction. 
		     (setf (slot-value instance slot-name)
			   (funcall (slot-definition-initfunction slot)))))))
    ;; Store the class slots in the instance so that we can update the instance
    ;; when the class changes.
    (setf (standard-instance-access instance +instance-slots-offset+)
	  class-slots)
    ;; Assign a timestamp to the instance so that we know whether it
    ;; is up to date.
    (setf (standard-instance-access instance +timestamp-offset+)
	  *next-available-timestamp*)
    (incf *next-available-timestamp*))
  instance)

(defmethod initialize-built-in-instance
    (instance &rest initargs &key &allow-other-keys)
  (let ((slots (effective--slots (class-of instance))))
    (loop for slot in slots
	  for slot-name = (slot-definition-name slot)
	  for location = (slot-definition-location slot)
	  do (multiple-value-bind (key value foundp)
		 ;; Find the first key/value pair in initargs where the
		 ;; key is one of the initargs of the slot. 
		 (get-properties initargs (slot-definition-initargs slot))
	       (declare (ignore key))
	       (if foundp
		   ;; Found an explicit initarg in initargs.  Initialize
		   ;; the slot from its value
		   (if (numberp location)
		       ;; The slot has allocation :instance, so the
		       ;; location is an offset into the slot vector.
		       (setf (slot-contents (heap-instance-slots instance)
					    location)
			     value)
		       ;; The slot has allocation :class, so the
		       ;; location is a CONS cell where the CAR
		       ;; contains the value.
		       (setf (car location) value))
		   ;; No explicit initarg found.  
		   (if (numberp location)
		       ;; The slot has allocation :instance, so the
		       ;; location is an offset into the slot vector.
		       (when (and (eq (slot-contents
				       (heap-instance-slots instance) location)
				      *secret-unbound-value*)
				  (not (null (slot-definition-initfunction slot))))
			 ;; Evaluate the initform by executing the
			 ;; initfunction.
			 (setf (slot-contents (heap-instance-slots instance)
					      location)
			       (funcall (slot-definition-initfunction slot))))
		       ;; The slot has allocation :class, so the
		       ;; location is a CONS cell where the CAR
		       ;; contains the value.
		       (when (and (eq (car location) *secret-unbound-value*)
				  (not (null (slot-definition-initfunction slot))))
			 ;; Evaluate the initform by executing the
			 ;; initfunction.
			 (setf (slot-contents (heap-instance-slots instance)
					      location)
			       (funcall (slot-definition-initfunction slot)))))))))
  instance)
  
