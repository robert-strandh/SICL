(cl:in-package #:sicl-new-boot-phase-1)

(defun load-file (file environment)
  (sicl-minimal-extrinsic-environment:cst-load-file file environment nil))

(defun import-function-from-host (name environment)
  (sicl-minimal-extrinsic-environment:import-function-from-host name environment))

(defun import-functions-from-host (names environment)
  (loop for name in names
        do (import-function-from-host name environment)))

(defun import-package-from-host (name environment)
  (sicl-minimal-extrinsic-environment:import-package-from-host name environment))
