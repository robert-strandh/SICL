(cl:in-package #:sicl-boot)

(defun fill-boot (boot)
  (customize-c1 boot)
  (customize-r1 boot)
  (phase1 boot)
  (customize-r2 boot)
  (customize-c2 boot)
  (phase2 boot)
  (phase3 boot))

(defmethod initialize-instance :after ((boot boot) &key &allow-other-keys)
  (fill-boot boot))
