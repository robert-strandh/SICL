(cl:in-package #:sicl-boot-phase-7)

(defun patch-method-combination (method-combination e5)
  (let ((standard-method-combination-class-e5
          ;; FIXME, make this STANDARD-METHOD-COMBINATION once we have it.
          (sicl-genv:find-class 'method-combination e5)))
    (setf (slot-value method-combination 'sicl-boot::%class)
          standard-method-combination-class-e5)))
