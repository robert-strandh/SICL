(cl:in-package #:cleavir-kildall-escape)

;;; Condition signaled for incorrect DYNAMIC-EXTENT declarations.
;;; FIXME: Seriously needs source reporting.
(define-condition incorrect-dynamic-extent
    (warning acclimation:condition)
  ((%instruction :initarg :instruction :reader instruction)))
