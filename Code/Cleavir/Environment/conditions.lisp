(cl:in-package #:cleavir-environment)

(define-condition no-info
    (program-error acclimation:condition)
  ((%name :initarg :name :reader name)
   (%origin :initarg :origin :reader origin)))

(define-condition no-variable-info (no-info)
  ())

(define-condition no-function-info (no-info)
  ())

(define-condition no-block-info (no-info)
  ())

(define-condition no-tag-info (no-info)
  ())
