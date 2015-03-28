(cl:in-package #:sicl-data-and-control-flow)

(define-condition odd-number-of-arguments-to-setf
    (program-error cleavir-i18n:condition)
  ((%form :initarg :form :reader form)))
