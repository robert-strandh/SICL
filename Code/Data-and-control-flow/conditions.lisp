(cl:in-package #:sicl-data-and-control-flow)

(define-condition odd-number-of-arguments-to-setf
    (program-error acclimation:condition)
  ((%form :initarg :form :reader form)))

(define-condition odd-number-of-arguments-to-psetf
    (program-error acclimation:condition)
  ((%form :initarg :form :reader form)))

(define-condition odd-number-of-arguments-to-psetq
    (program-error acclimation:condition)
  ((%form :initarg :form :reader form)))

(define-condition too-few-arguments-to-shiftf
    (program-error acclimation:condition)
  ((%form :initarg :form :reader form)))

(define-condition attempt-to-set-the-fdefinition-of-a-special-operator
    (error acclimation:condition)
  ((name :initarg :name :reader name)))
