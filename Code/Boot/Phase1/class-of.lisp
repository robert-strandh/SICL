(cl:in-package #:sicl-boot-phase1)

(defun class-of (object)
  (if (heap-instance-p object)
      (heap-instance-class object)
      *t*))
