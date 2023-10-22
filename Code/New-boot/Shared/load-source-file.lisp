(cl:in-package #:sicl-new-boot)

(defun load-stream (client stream environment)
  (let ((eclector.base:*client* client)
        (abp:*builder* (cb::make-builder client environment)))
    (loop with eof = (list nil)
          for cst = (eclector.concrete-syntax-tree:read stream nil eof)
          until (eq cst eof)
          do (let ((ast (cst-to-ast client cst environment)))
               (cbae:eval-ast client ast environment)))))

(defun load-source-file (client filename environment)
  (format *trace-output* "Loading ~s into ~s~%"
          filename environment)
  (finish-output *trace-output*)
  (sicl-source-tracking:with-source-tracking-stream-from-file (stream filename)
    (load-stream client stream environment)))
