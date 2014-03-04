(cl:in-package #:sicl-boot-phase1)

(defun satiate-bridge-generic-functions ()
  (loop for entry in *bridge-generic-functions*
	do (satiate-generic-function (cdr entry))))
