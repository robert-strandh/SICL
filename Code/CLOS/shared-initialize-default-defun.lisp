(cl:in-package #:sicl-clos)

;;; We separate this function from the main workhorse named
;;; SHARED-INITIALIZE-DEFAULT-USING-CLASS-AND-SLOTS in the file
;;; shared-initialize-support.lisp because, during bootstrapping
;;; it will be defined with a special version that spans two
;;; environments.
(defun shared-initialize-default (instance slot-names &rest initargs)
  (let* ((class (class-of instance))
         (slots (class-slots class)))
    (apply #'shared-initialize-default-using-class-and-slots
           instance slot-names class slots initargs)))
