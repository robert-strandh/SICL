(cl:in-package #:sicl-boot-phase-2)

(defun enable-class-initialization (boot)
  (with-accessors ((e3 sicl-boot:e3)) boot
    (defmethod initialize-instance :around ((class funcallable-standard-class)
                                            &rest arguments
                                            &key
                                              direct-default-initargs
                                              direct-superclasses
                                              direct-slots
                                            &allow-other-keys)
      (let ((new-direct-slots
              (loop for slot-spec in direct-slots
                    for spec = (copy-list slot-spec)
                    do (remf spec :readers)
                       (remf spec :writers)
                    collect spec)))
        (apply #'call-next-method
               class
               :direct-superclasses direct-superclasses
               :direct-default-initargs direct-default-initargs
               :direct-slots new-direct-slots
               arguments)
        (loop for slot-spec in direct-slots
              for slot = (apply #'make-instance
                                'closer-mop:standard-direct-slot-definition
                                slot-spec)
              for slot-name = (getf slot-spec :name)
              for readers = (getf slot-spec :readers)
              for writers = (getf slot-spec :writers)
              do (add-readers e3 readers class slot-name slot)
                 (add-writers e3 writers class slot-name slot))))))
