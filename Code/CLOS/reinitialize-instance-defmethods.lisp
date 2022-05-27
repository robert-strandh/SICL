(cl:in-package #:sicl-clos)

(defmethod reinitialize-instance
    ((instance standard-object) &rest initargs &key &allow-other-keys)
  ;; Call shared-initialize with a slot-list of (), meaning no slot,
  ;; i.e., only assign values to slots that have explicit
  ;; initialization arguments in initargs.
  (apply #'shared-initialize instance () initargs))

(defmethod reinitialize-instance :around
    ((class real-class)
     &rest initargs
     &key (direct-superclasses '() direct-superclasses-p)
     &allow-other-keys)
  (let ((existing-direct-superclasses (class-direct-superclasses class)))
    (check-direct-superclasses class direct-superclasses)
    (when direct-superclasses-p
      (when (null direct-superclasses)
        (setf direct-superclasses (default-superclasses class)))
      (loop with difference = (set-difference existing-direct-superclasses
                                              direct-superclasses)
            for removed-superclass in difference
            do (remove-direct-subclass removed-superclass class)))
    (if direct-superclasses-p
        (apply #'call-next-method
               class
               :direct-superclasses direct-superclasses
               initargs)
        (call-next-method))
    (when direct-superclasses-p
      (loop with difference = (set-difference direct-superclasses
                                              existing-direct-superclasses)
            for added-superclass in difference
            do (add-direct-subclass added-superclass class))))
  class)
