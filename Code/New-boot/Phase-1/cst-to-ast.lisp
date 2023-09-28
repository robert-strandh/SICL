(cl:in-package #:sicl-new-boot)

(defun cst-to-ast (client cst environment)
  (let ((cmd:*client* client))
    (cb:cst-to-ast client cst environment)))
