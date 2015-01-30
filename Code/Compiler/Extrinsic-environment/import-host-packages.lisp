;;; Fill the target environment with all available packages in the host.
(defun import-host-packages (environment)
  (setf (sicl-env:packages environment)
	(list-all-packages)))
