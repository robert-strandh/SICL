(cl:in-package #:sicl-boot-phase-3)

;;; The problem we solve with this code is that we create host classes
;;; using the host MAKE-INSTANCE function.  Doing so will trigger that
;;; class-initialization protocol.  Part of that protocol involves
;;; adding slot readers and slot writers to generic functions, and
;;; those functions are determined by their names.  As part of that
;;; protocol, those names are used to look up generic functions in the
;;; host global environment, but that is not what we want.  Our
;;; generic functions are located in environment E3.  We solve the
;;; problem by introducing an :AROUND method on the host function
;;; INITIALIZE-INSTANCE.  This :AROUND method first removes all
;;; keyword/value pairs with keys :READERS and :WRITERS from all the
;;; canonical slot specifications, before calling the main method on
;;; INITIALIZE-INSTANCE, which then prevents methods from being added
;;; to host generic functions in the host global environment.  Next,
;;; it adds those readers and writers explicitly to our generic
;;; functions in E3, using special-purpose code.
(defun enable-class-initialization (e3)
  (defmethod initialize-instance :around
      ((class sicl-boot-phase-2::funcallable-standard-class)
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
                              'sicl-host-mop:standard-direct-slot-definition
                              slot-spec)
            for slot-name = (getf slot-spec :name)
            for readers = (getf slot-spec :readers)
            for writers = (getf slot-spec :writers)
            do (add-readers e3 readers class slot-name slot)
               (add-writers e3 writers class slot-name slot)))))
