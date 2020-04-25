(cl:in-package #:sicl-clos)

;;; We extract the body of the default methods into separate functions
;;; in a separate file.  During bootstrapping, the generic functions
;;; need to be tied to and present in an environment En, but the
;;; classes need to be looked up in the environment En-1.  We
;;; accomplish this effect by having READER-METHOD-CLASS-DEFAULT and
;;; WRITER-METHOD-CLASS-DEFAULT tied to En-1 and present in En.

(defun reader-method-class-default (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))

(defun writer-method-class-default (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))
