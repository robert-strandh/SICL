(cl:in-package #:cleavir-environment)

(define-condition no-variable-info
    (program-error cleavir-i18n:condition)
  ((%name :initarg :name :reader name)))

(define-condition no-function-info
    (program-error cleavir-i18n:condition)
  ((%name :initarg :name :reader name)))

(define-condition no-block-info
    (program-error cleavir-i18n:condition)
  ((%name :initarg :name :reader name)))

(define-condition no-tag-info
    (program-error cleavir-i18n:condition)
  ((%name :initarg :name :reader name)))
