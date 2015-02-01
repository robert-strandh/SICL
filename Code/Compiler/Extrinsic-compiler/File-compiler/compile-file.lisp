(cl:in-package #:sicl-extrinsic-file-compiler)

(defun ast-from-stream (stream)
  (loop with eof = (list nil)
	for form = (sicl-reader:read stream nil eof)
	until (eq form eof)
	collect form))
