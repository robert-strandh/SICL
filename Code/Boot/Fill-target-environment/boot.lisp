(cl:in-package #:sicl-boot-fill-target-environment)

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client sicl-client:*client*)
           (make-instance (env:fdefinition client e5 'make-instance))
           (target-environment (funcall make-instance 'env:run-time-environment))
           (target-fdefinition (env:fdefinition client e5 'env:fdefinition))
           (client (make-instance 'client
                     :target-environment target-environment
                     :target-fdefinition target-fdefinition))
           (sicl-client:*client* client))
      client)))
