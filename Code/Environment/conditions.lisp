(cl:in-package #:sicl-standard-environment-functions)

(define-condition no-such-class ()
  ((%name :initarg :name :reader name)))
