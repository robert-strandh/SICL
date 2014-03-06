(cl:in-package #:sicl-clos)

(defun ensure-class (&rest arguments)
  (apply #'sicl-boot-phase2:*ensure-class arguments))

(defun ensure-built-in-class (&rest arguments)
  (apply #'sicl-boot-phase2:*ensure-built-in-class arguments))

(defun ensure-generic-function (&rest arguments)
  (apply #'sicl-boot-phase2:*ensure-generic-function arguments))

(defun ensure-method (&rest arguments)
  (apply #'sicl-boot-phase2:*ensure-method arguments))
