(cl:in-package #:sicl-new-boot-phase-0)

(defun load-file (file environment)
  (sicl-minimal-extrinsic-environment:cst-load-file file environment nil))
