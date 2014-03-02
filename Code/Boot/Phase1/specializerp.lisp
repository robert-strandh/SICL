(cl:in-package #:sicl-boot-phase1)

(defun specializerp (object)
  (typep object 'cl:class))
