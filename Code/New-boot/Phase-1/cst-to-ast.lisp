(cl:in-package #:sicl-new-boot-phase-1)

(defun cst-to-ast (client cst environment)
  (let ((cmd:*client* client))
    (cb:cst-to-ast client cst environment)))
