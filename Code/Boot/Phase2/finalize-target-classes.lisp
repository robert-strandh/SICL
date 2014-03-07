(cl:in-package #:sicl-boot-phase2)

(defun finalize-target-classes ()
  (loop for entry in *target-classes*
	do (finalize-inheritance (cdr entry))))
