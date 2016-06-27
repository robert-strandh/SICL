(cl:in-package #:cleavir-environment)

(define-condition no-variable-info
    (program-error acclimation:condition)
  ((%name :initarg :name :reader name)))

(define-condition no-function-info
    (program-error acclimation:condition)
  ((%name :initarg :name :reader name)))

(define-condition no-block-info
    (program-error acclimation:condition)
  ((%name :initarg :name :reader name)))

(define-condition no-tag-info
    (program-error acclimation:condition)
  ((%name :initarg :name :reader name)))
