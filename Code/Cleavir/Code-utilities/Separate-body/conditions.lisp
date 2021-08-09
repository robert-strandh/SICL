(cl:in-package #:cleavir-code-utilities)

(define-condition ordinary-body-must-be-proper-list
    (style-warning acclimation:condition)
  ((%body :initarg :body :reader body)))

(define-condition function-body-must-be-proper-list
    (style-warning acclimation:condition)
  ((%body :initarg :body :reader body)))
