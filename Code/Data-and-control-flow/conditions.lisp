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

(define-condition numeric-catch-tag (style-warning)
  ((%tag :initarg :tag :reader tag))
  (:report (lambda (condition stream)
             (format stream
                     "CATCH tags are compared with EQ so using a~@
                      number or a character as a CATCH tag may not~@
                      work as expected:~@
                      ~s"
                     (tag condition)))))
