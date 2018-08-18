(cl:in-package #:sicl-new-boot)

(defun make-environment ()
  (make-instance 'sicl-minimal-extrinsic-environment:environment))

(defclass boot ()
  ((%e1 :initarg :e1 :accessor e1)
   (%e2 :initarg :e2 :accessor e2)
   (%e3 :initarg :e3 :accessor e3)))

(defun boot ()
  (let ((boot (make-instance 'boot
                :e1 (make-environment)
                :e2 (make-environment))))
    (sicl-new-boot-phase-1:boot-phase-1 boot)))
