(cl:in-package #:sicl-boot)

(defun fill-boot (boot)
  (customize-environments boot)
  (phase1 boot)
  (phase2 boot)
  (phase3 boot))

(defmethod initialize-instance :after ((boot boot) &key &allow-other-keys)
  (fill-boot boot))
