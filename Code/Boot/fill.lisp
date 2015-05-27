(cl:in-package #:sicl-boot)

(defun fill-boot (boot)
  (declare (ignore boot))
  ())

(defmethod initialize-instance :after ((boot boot) &key &allow-other-keys)
  (fill-boot boot))
