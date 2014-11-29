(cl:in-package #:sicl-standard-environment-functions)

(define-condition no-such-class (error)
  ((%name :initarg :name :reader name)))
