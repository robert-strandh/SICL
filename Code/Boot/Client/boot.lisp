(cl:in-package #:sicl-boot-client)

(defclass client (sicl-boot:client) ())

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (ensure-asdf-system-using-client client e5 '#:sicl-client))))
