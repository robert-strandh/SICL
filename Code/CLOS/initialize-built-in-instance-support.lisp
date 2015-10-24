(in-package #:sicl-clos)

;;; This function implements the default method of the generic
;;; function INITIALIZE-BUILT-IN-INSTANCE.  That method is not
;;; specialized, so it is applicable to any instance.
;;;
;;; The default method of INITIALIZE-BUILT-IN-INSTANCE behaves in a
;;; way similar to that of SHARED-INITIALIZE (when called from
;;; INITIALIZE-INSTANCE) for standard objects.  But since built-in
;;; classes can not be redefined, built-in instances can not become
;;; obsolete, so we have no need for the equivalent of
;;; REINITIALIZE-INSTANCE for built-in instances.  And since
;;; SHARED-INITIALIZE exists to capture commonalities between
;;; INITIALIZE-INSTANCE and REINITIALIZE-INSTANCE, we do not need the
;;; equivalent of SHARED-INITIALIZE either, and we simply put all that
;;; functionality in INITIALIZE-BUILT-IN-INSTANCE.
;;;
;;; The main difference between what we do here and what
;;; SHARED-INITIALIZE does, is that SHARED-INITIALIZE calls
;;; SLOT-BOUNDP-USING-CLASS and (SETF SLOT-VALUE-USING-CLASS) so that
;;; subclasses of STANDARD-CLASS and
;;; STANDARD-EFFECTIVE-SLOT-DEFINITION can be taken into account.
;;; Here, however, we do now allow for subclasses of BUILT-IN-CLASS
;;; nor of STANDARD-EFFECTIVE-SLOT-DEFINITION, so we can safely assume
;;; that these are the exact classes we have to deal with.

(defun initialize-built-in-instance-default
    (instance &rest initargs &key &allow-other-keys)
  (let* ((class (class-of instance))
	 (slots (class-slots class)))
    (loop for slot in slots
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
				      +unbound-slot-value+)
				  (not (null (slot-definition-initfunction slot))))
			 ;; Evaluate the initform by executing the
			 ;; initfunction.
			 (setf (slot-contents (heap-instance-slots instance)
					      location)
			       (funcall (slot-definition-initfunction slot))))
		       ;; The slot has allocation :class, so the
		       ;; location is a CONS cell where the CAR
		       ;; contains the value.
		       (when (and (eq (car location) +unbound-slot-value+)
				  (not (null (slot-definition-initfunction slot))))
			 ;; Evaluate the initform by executing the
			 ;; initfunction.
			 (setf (slot-contents (heap-instance-slots instance)
					      location)
			       (funcall (slot-definition-initfunction slot)))))))))
  instance)
