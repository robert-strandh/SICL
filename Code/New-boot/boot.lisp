(cl:in-package #:sicl-new-boot)

(defun boot ()
  (let ((boot (make-instance 'boot)))
    (apply #'values
           boot
           (multiple-value-list (sicl-new-boot-phase-1:boot boot)))))
