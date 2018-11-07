(cl:in-package #:sicl-new-boot-phase-2)

;;; Environment E2.
;;;
;;; In this environment, generic functions are host generic functions
;;; of type STANDARD-GENERIC-FUNCTION.  Methods on these generic
;;; functions are host methods of type STANDARD-METHOD.
;;;
;;; Classes in this environment are bridge classes.

(defclass environment (sicl-new-boot:environment)
  ())

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'generic-function)) (environment environment))
  t)

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  t)
