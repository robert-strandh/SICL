(cl:in-package #:sicl-boot-phase2)

(defun ensure-accessor-function (name lambda-list)
  (*ensure-generic-function name :lambda-list lambda-list))
