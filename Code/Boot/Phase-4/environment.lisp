(cl:in-package #:sicl-boot-phase-4)

;;; Environment E4.
;;;
;;; In this environment, generic functions are bridge generic
;;; functions of type STANDARD-GENERIC-FUNCTION.  Methods on these
;;; generic functions are bridge methods of type STANDARD-METHOD.
;;;
;;; Classes in this environment are ersatz classes.

(defclass environment (sicl-boot:environment)
  ())
