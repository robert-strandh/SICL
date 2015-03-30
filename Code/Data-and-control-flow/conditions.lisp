(cl:in-package #:sicl-data-and-control-flow)

(define-condition odd-number-of-arguments-to-setf
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)))

(define-condition odd-number-of-arguments-to-psetf
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)))

(define-condition odd-number-of-arguments-to-psetq
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)))

(define-condition too-few-arguments-to-shiftf
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)))
