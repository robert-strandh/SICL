(cl:in-package #:sicl-boot-phase1)

(defun finalize-bridge-classes ()
  (loop for entry in *bridge-classes*
	do (finalize-inheritance (cdr entry))))
