(cl:in-package #:cleavir-load-time-value-hoisting)

(define-condition non-externalizable-literal-object
    (error acclimation:condition)
  ((%literal-object :initarg :literal-object :reader literal-object)))

(define-condition circular-dependencies-in-creation-form
    (error acclimation:condition)
  ((%object :initarg :object :reader object)
   (%creation-form :initarg :creation-form :reader creation-form)))
