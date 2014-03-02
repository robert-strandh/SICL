(cl:in-package #:sicl-boot-phase1)

(defun classp (object)
  (typep object 'cl:class))
