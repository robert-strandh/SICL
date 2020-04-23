(cl:in-package #:sicl-boot)

(defclass environment (sicl-extrinsic-environment:environment)
  ()
  (:default-initargs :client (make-instance 'client)))

(defmethod initialize-instance :after ((env environment) &key)
  (setf (sicl-genv:fdefinition 'sicl-genv:global-environment env)
        (lambda (&optional lexical-environment)
          (if (null lexical-environment)
              env
              (trucler:global-environment
               (sicl-genv:client env) lexical-environment)))))
