(cl:in-package #:sicl-new-boot)

(defun make-environment ()
  (make-instance 'sicl-minimal-extrinsic-environment:environment))

(defclass boot ()
  ((%e0 :initarg :e0 :accessor e0)
   (%e1 :initarg :e1 :accessor e1)
   (%e2 :initarg :e2 :accessor e2)
   (%e3 :initarg :e3 :accessor e3)
   (%e4 :initarg :e4 :accessor e4)))

(defun boot ()
  (let ((boot
          (let ((sicl-minimal-extrinsic-environment::*cache-p* t))
            (make-instance 'boot
              :e0 (make-environment)
              :e1 (make-environment)
              :e2 (make-environment)
              :e3 (make-environment)
              :e4 (make-environment)))))
    (sicl-new-boot-phase-0:boot-phase-0 boot)
    (sicl-new-boot-phase-1:boot-phase-1 boot)
    (sicl-new-boot-phase-2:boot-phase-2 boot)
    (sicl-new-boot-phase-3:boot-phase-3 boot)
    boot))
