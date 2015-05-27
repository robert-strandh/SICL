(cl:in-package #:sicl-boot)

(defun fill-boot (boot)
  (fill1 boot))

(defmethod initialize-instance :after ((boot boot) &key &allow-other-keys)
  (fill-boot boot))
