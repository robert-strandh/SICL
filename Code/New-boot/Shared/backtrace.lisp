(cl:in-package #:sicl-new-boot)

(defun bt ()
  (let* ((package (find-package '#:sicl-new-boot-backtrace-inspector))
         (inspect-symbol (find-symbol "INSPECT" package)))
    (funcall inspect-symbol cbe:*call-stack*)))
