(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, SHARED-INITIALIZE.

;;; The spec requires the method on initialize-instance and
;;; reinitialize-instance specialized for standard-object to call
;;; shared-initialize as shown below.  The slot-name argument is a
;;; list of slot-names to be initialized, or t which means all slots.
;;; A slot is initialized from the initargs if one of its initargs is
;;; in that list.  Otherwise it is initialized by evaluating the
;;; corresponding intitform if and only if it is one of the slots to
;;; be initialized (according to the value of slot-names), and if it
;;; does not already have a value.  In that case, if no initform
;;; exists, the slot is left unbound.

;;; We do not want to use SLOT-BOUNDP (SETF SLOT-VALUE) here.  Given
;;; the name of a slot, those functions find a slot metaobject with
;;; that name, and then call the ...-USING-CLASS version of the
;;; function.  The main difficulty for those functions is to find the
;;; slot metaobject.  In SHARED-INITIALIZE, we already have the slot
;;; metaobject available, so we use the ...-USING-CLASS version
;;; directly.

(defun shared-initialize-default
    (instance slot-names &rest initargs)
  (let* ((class (class-of instance))
	 (class-slots (class-slots class)))
    (loop for slot in class-slots
	  do (multiple-value-bind (key value foundp)
		 ;; Find the first key/value pair in initargs where the
		 ;; key is one of the initargs of the slot. 
		 (get-properties initargs (slot-definition-initargs slot))
	       (declare (ignore key))
	       (if foundp
		   ;; Found an explicit initarg in initargs.
		   ;; Initialize the slot from its value.
		   (setf (slot-value-using-class class instance slot)
			 value)
		   ;; No explicit initarg found.  
		   (when (and (not (slot-boundp-using-class class instance slot))
			      (not (null (slot-definition-initfunction slot)))
			      (or (eq slot-names t)
				  (member (slot-definition-name slot)
					  slot-names)))
		     ;; Evaluate the initform by executing the initfunction. 
		     (setf (slot-value-using-class class instance slot)
			   (funcall (slot-definition-initfunction slot)))))))
    ;; Store the class slots in the instance so that we can update the instance
    ;; when the class changes.
    (setf (standard-instance-access instance +instance-slots-offset+)
	  class-slots)
    ;; Store the unique number of the class in the instance so that we can
    ;; recognize when the instance is obsolete.
    (setf (standard-instance-access instance +class-unique-number-offset+)
	  (unique-number class)))
  instance)

;;; The method on initialize-instance specialized for standard-object
;;; is especially important to bootstrapping, because all important
;;; metaobject classes are subclasses of standard-object.  So if
;;; make-instance is used, directly or indirectly (using ensure-class)
;;; to create a metaobject class, then this method must already be in
;;; place.

(defun initialize-instance-default
    (instance &rest initargs &key &allow-other-keys)
  ;; Call shared-initialize with a slot-list of t, meaning all slots,
  ;; i.e., for every slot that is not explicitly initialized and which
  ;; is unbound, evaluate its initform if it has one. 
  (apply #'shared-initialize instance t initargs))

(defun reinitialize-instance-default
    (instance &rest initargs &key &allow-other-keys)
  ;; Call shared-initialize with a slot-list of (), meaning no slot,
  ;; i.e., only assign values to slots that have explicit
  ;; initialization arguments in initargs. 
  (apply #'shared-initialize instance () initargs))

