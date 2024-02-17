(cl:in-package #:sicl-new-boot)

(defclass environment (sicl-environment:run-time-environment)
  ((%name :initarg :name :reader name)))

(defmethod print-object ((object environment) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (name object))))
