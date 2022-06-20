(cl:in-package #:sicl-boot-phase-7)

(defclass client (sicl-boot:client) ())

(defun boot (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e* sicl-boot:e*))
      boot
    (let ((sicl-client:*client* (make-instance 'client)))
      (setf e*
            (funcall (env:fdefinition sicl-client:*client* e5 'make-instance)
                     'env:run-time-environment))
      (change-class e* 'sicl-boot:run-time-environment-header))))
