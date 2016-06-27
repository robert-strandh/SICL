(cl:in-package #:sicl-evaluation-and-compilation)

(define-condition environment-must-be-omitted-or-nil
    (error acclimation:condition)
  ((%environment :initarg :environment :reader environment)))
