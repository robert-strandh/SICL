(cl:in-package #:sicl-boot)

(defun boot ()
  (let ((boot (make-instance 'boot)))
    (sicl-boot-phase-0:boot boot)
    (sicl-boot-phase-1:boot boot)
    boot))
