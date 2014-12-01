(cl:in-package #:sicl-evaluation-and-compilation)

(define-condition environment-must-be-omitted-or-nil
    (error cleavir-i18n:condition)
  ((%environment :initarg :environment :reader environment)))
