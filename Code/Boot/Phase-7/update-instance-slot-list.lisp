(cl:in-package #:sicl-boot-phase-7)

;;; Every standard object has a list of slot metaobjects as element
;;; number 1 in the rack.  This list is the list of the effective slot
;;; definition objects of the class, at the moment the instance was
;;; created.  Therefore, those slot metaobjects have a lower purity
;;; than the standard object itself.
;;;
;;; During bootstrapping, this list is not used for anything
;;; particular.  It is meant to be used by the garbage collector, and
;;; also with UPDATE-INSTANCE-FOR-CHANGED-CLASS.  However, it still
;;; needs to be updated so that the graph of metaobjects is circular.
;;;
;;; We accomplish the task by waiting until every standard object in
;;; environment E5 has been updated in every respect except the list
;;; of slot metaobjects.  In particular, we wait until the class of
;;; every standard object in E5 is a class also in E5.  Then we simply
;;; copy the effective slot definitions of the class into the rack of
;;; the instance.  Recall that the effective slot definitions of the
;;; class can be accessed by calling CLASS-SLOTS.

(defun update-instance-slot-list (instance e5)
  (let* ((class (slot-value instance 'sicl-boot::%class))
         (fun (sicl-genv:fdefinition 'sicl-clos:class-slots e5))
         (slots (funcall fun class)))
    (setf (aref (slot-value class 'sicl-boot::%rack) 1)
          slots)))
