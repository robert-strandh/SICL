(cl:in-package #:sicl-new-boot-phase-4)

;;; Environment E4.
;;;
;;; In this environment, generic functions are ersatz generic
;;; functions of type STANDARD-GENERIC-FUNCTION.  Methods on these
;;; generic functions are ersatz methods of type STANDARD-METHOD.
;;;
;;; Classes in this environment are ersatz classes with ersatz
;;; superclasses.

(defclass environment (sicl-new-boot:environment)
  ())

;; (defmethod sicl-genv:typep
;;     (object (type-specifier (eql 'function)) (environment environment))
;;   (typep object 'function))

;; (defmethod sicl-genv:typep
;;     (object (type-specifier (eql 'class)) (environment environment))
;;   (not (symbolp object)))
