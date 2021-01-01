(cl:in-package #:cleavir-value-hoisting)

(define-condition circular-dependencies-in-creation-form
    (error acclimation:condition)
  ((%object :initarg :object :reader object)
   (%creation-form :initarg :creation-form :reader creation-form)))

(define-condition object-not-externalizable
    (error acclimation:condition)
  ((%object :initarg :object :reader object)))
