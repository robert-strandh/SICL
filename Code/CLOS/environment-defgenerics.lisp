(cl:in-package #:sicl-clos)

;; Given an instance of the class FUNCALLABLE-STANDARD-OBJECT, return
;; its static environment.
(defgeneric environment (funcallable-standard-object))

;; Given a vector and an instance of the class
;; FUNCALLABLE-STANDARD-OBJECT, make that vector the static
;; environment of that object.
(defgeneric (setf environment) (environment funcallable-standard-object))
