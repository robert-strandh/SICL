(cl:in-package #:sicl-clos)

(defun copy-instance (instance)
  (let* ((class (class-of instance))
         (copy (allocate-instance class)))
    (loop for i from 1 below (+ (instance-size class) 2)
          do (setf (standard-instance-access copy i)
                   (standard-instance-access instance i)))
    copy))

(defun initialize-new-instance (original from-class new to-class)
  (let ((from-class-slots (class-slots from-class))
        (to-class-slots (class-slots to-class)))
    (loop for to-slot in to-class-slots
          for name = (slot-definition-name to-slot)
          for from-slot
            = (find name from-class-slots :test #'eq :key #'slot-definition-name)
          do (when (and (not (null from-slot))
                        (slot-boundp-using-class from-class original from-slot))
               (setf (slot-value-using-class to-class new to-slot)
                     (slot-value-using-class from-class original from-slot))))))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_chg_cl.htm#change-class
(defgeneric change-class (instance new-class &key &allow-other-keys))

(defmethod change-class (instance (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defmethod change-class ((instance standard-object) (new-class standard-class) &rest initargs)
  (unless (class-finalized-p new-class)
    (finalize-inheritance new-class))
  (let ((copy (copy-instance instance))
        (new (allocate-instance new-class)))
    (setf (standard-instance-access new 1)
          (class-slots new-class))
    (initialize-new-instance instance (class-of instance) new new-class)
    (update-header instance new)
    (apply #'update-instance-for-different-class copy new initargs)
    new))
