(cl:in-package #:cleavir-code-utilities)

(define-condition code-condition ()
  ((%code :initarg :code :reader code)))
