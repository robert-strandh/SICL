(cl:in-package #:sicl-boot)

(defclass environment (sicl-extrinsic-environment:environment)
  ((%name :initarg :name :reader name))
  (:default-initargs :client (make-instance 'client)))

(defmethod print-object ((object environment) stream)
  (print-unreadable-object (object stream)
    (format stream "Environment ~a" (name object))))

(defmethod initialize-instance :after ((env environment) &key)
  (setf (sicl-genv:fdefinition 'sicl-genv:global-environment env)
        (lambda (&optional lexical-environment)
          (if (null lexical-environment)
              env
              (trucler:global-environment
               (sicl-genv:client env) lexical-environment)))))
