(cl:in-package #:sicl-extrinsic-environment)

(defun load-file (filename environment)
  (load (asdf:system-relative-pathname :sicl-extrinsic-hir-compiler filename)
	environment))
