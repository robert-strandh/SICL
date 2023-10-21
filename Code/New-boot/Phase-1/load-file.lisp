(cl:in-package #:sicl-new-boot-phase-1)

(defun load-stream (client stream environment)
  (let ((eclector.base:*client* client)
        (abp:*builder* (cb::make-builder client environment)))
    (loop with eof = (list nil)
          for cst = (eclector.concrete-syntax-tree:read stream nil eof)
          until (eq cst eof)
          do (let ((ast (cst-to-ast client cst environment)))
               (cbae:eval-ast client ast environment)))))

(defun load-file (client filename environment)
  (sicl-source-tracking:with-source-tracking-stream-from-file (stream filename)
    (load-stream client stream environment)))
