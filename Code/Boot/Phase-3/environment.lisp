(cl:in-package #:sicl-boot-phase-3)

;;; Environment E3.
;;;
;;; In this environment, generic functions are bridge generic
;;; functions of type STANDARD-GENERIC-FUNCTION.  Methods on these
;;; generic functions are bridge methods of type STANDARD-METHOD.
;;;
;;; Classes in this environment are ersatz classes.

(defclass environment (sicl-boot:environment)
  ())

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'function)) (environment environment))
  (typep object 'function))

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  (not (symbolp object)))
