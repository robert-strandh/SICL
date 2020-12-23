(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_update.htm#update-instance-for-different-class
(defgeneric update-instance-for-different-class
    (previous current
     &rest initargs
     &key
     &allow-other-keys))

(defmethod update-instance-for-different-class
    ((previous standard-object) (current standard-object) &rest initargs)
  (let ((from-class-slots (class-slots (class-of previous)))
        (to-class-slots (class-slots (class-of current)))
        (added-slot-names '()))
    (loop for to-slot in to-class-slots
          for name = (slot-definition-name to-slot)
          do (when (and (eq (slot-definition-allocation to-slot) :instance)
                        (null (find name from-class-slots
                                    :test #'eq
                                    :key #'slot-definition-name)))
               (push name added-slot-names)))
    (apply #'shared-initialize current added-slot-names initargs)))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_upda_1.htm#update-instance-for-redefined-class
(defgeneric update-instance-for-redefined-class
    (instance added-slots discarded-slots property-list
     &rest initargs
     &key
     &allow-other-keys))
