(cl:in-package #:sicl-boot-phase2)

(defun finalize-ersatz-classes ()
  (loop for entry in *ersatz-classes*
	do (finalize-inheritance (cdr entry))))
