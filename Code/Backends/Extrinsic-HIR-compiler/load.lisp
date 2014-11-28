(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun load (file)
  (with-open-file (stream file :direction :input)
    (loop with eof = (list nil)
	  for form = (sicl-reader:read stream nil eof)
	  until (eq form eof)
	  do (eval form))))
