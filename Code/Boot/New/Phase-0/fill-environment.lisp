(cl:in-package #:sicl-boot-phase-0)

(defun fill-environment (client environment)
  (declare (igore client))
  (import-from-host environment))
