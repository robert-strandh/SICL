(cl:in-package #:sicl-clos)

;;; We extract the body of the default methods into a separate
;;; function in a separate file.  During bootstrapping, the generic
;;; function need to be tied to and present in an environment En, but
;;; the classes need to be looked up in the environment En-1.  We
;;; accomplish this effect by having
;;; DIRECT-SLOT-DEFINITION-CLASS-DEFAULT tied to En-1 and present in
;;; En.

(defun direct-slot-definition-class-default (class &rest initargs)
  (declare (ignore class initargs))
  (find-class 'standard-direct-slot-definition))
