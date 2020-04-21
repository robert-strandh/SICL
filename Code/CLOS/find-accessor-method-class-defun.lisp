(cl:in-package #:sicl-clos)

;;; This definition should be loaded into the final system.  During
;;; bootstrapping, however, we define it differently, because the
;;; class needs to be found in a different environment.
(defun define-accessor-method-class (class-name)
  (find-class class-name))
