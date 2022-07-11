(cl:in-package #:sicl-boot-environment)

(defclass client (sicl-boot:client) ())

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (ensure-asdf-system-using-client client e5 '#:clostrum)
      (ensure-asdf-system-using-client client e5 '#:clostrum-basic)
      (ensure-asdf-system-using-client client e5 '#:sicl-environment))))
