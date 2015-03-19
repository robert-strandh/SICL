(cl:in-package #:sicl-extrinsic-environment)

(defun import-from-host (environment)
  ;; Import available packages in the host to ENVIRONMENT.
  (setf (sicl-env:packages environment)
	(list-all-packages))
  ;; Import available functions in the host to ENVIRONMENT.
  (do-all-symbols (symbol)
    (when (and (fboundp symbol)
	       (not (special-operator-p symbol))
	       (null (macro-function symbol)))
      (setf (sicl-global-environment:fdefinition symbol environment)
	    (fdefinition symbol)))
    (when (fboundp `(setf ,symbol))
      (setf (sicl-global-environment:fdefinition `(setf ,symbol) environment)
	    (fdefinition `(setf ,symbol))))))
