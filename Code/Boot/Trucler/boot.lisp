(cl:in-package #:sicl-boot-trucler)

(defclass client (sicl-boot:client) ())

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (let* ((client (make-instance 'client))
           (sicl-client:*client* client))
      (with-temporary-function-imports
          client e5 '(format)
        (ensure-asdf-system-using-client client e5 '#:trucler-base)
        (ensure-asdf-system-using-client client e5 '#:trucler-reference)))))
