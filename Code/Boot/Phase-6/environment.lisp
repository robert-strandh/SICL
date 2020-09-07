(cl:in-package #:sicl-boot-phase-6)

;;; Environment E6.
;;;
;;; In this environment, generic functions are ersatz generic
;;; functions of type STANDARD-GENERIC-FUNCTION.  Methods on these
;;; generic functions are ersatz methods of type STANDARD-METHOD.
;;;
;;; Classes in this environment are ersatz classes with ersatz
;;; metaclasses.

(defclass environment (sicl-boot:environment)
  ())
