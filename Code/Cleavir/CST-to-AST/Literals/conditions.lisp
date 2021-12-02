(cl:in-package #:cleavir-literals)

(define-condition circular-dependencies-in-creation-forms
    (program-error acclimation:condition)
  ((%creation-forms :initarg :creation-forms :reader creation-forms)))

(define-condition object-not-externalizable
    (program-error acclimation:condition)
  ((%object :initarg :object :reader object)))
