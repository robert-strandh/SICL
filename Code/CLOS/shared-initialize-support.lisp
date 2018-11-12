(cl:in-package #:sicl-clos)

;;; We do not want to use SLOT-BOUNDP and (SETF SLOT-VALUE) here.
;;; Given the name of a slot, those functions find a slot metaobject
;;; with that name, and then call the ...-USING-CLASS version of the
;;; function.  The main difficulty for those functions is to find the
;;; slot metaobject.  In SHARED-INITIALIZE, we already have the slot
;;; metaobject available, so we use the ...-USING-CLASS version
;;; directly.

(defun shared-initialize-default-using-class-and-slots
    (instance slot-names class slots &rest initargs)
  (loop for slot in slots
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
        slots)
  instance)
